local lpeg = require "lpeg"
local pt = require "pt"
local inspect = require "inspect"

local locale = lpeg.locale()

-- match-time capture based debugger
local function I (msg)
  return lpeg.P(function () print(msg); return true end)
end

local function node(tag, ...)
  local labels = table.pack(...)
  local params = table.concat(labels, ", ")
  local fields = string.gsub(params, "(%w+)", "%1 = %1")

  local code = string.format(
    "return function (%s) return { tag = \"%s\", %s } end",
    params, tag, fields
  )

  -- print(code)
  return assert(load(code))()
end

-- stmt2 is optional
local function nodeStmts (stmt1, stmt2)
  if stmt2 == nil then
    return stmt1
  else
    return { tag = "sequence", stmt1 = stmt1, stmt2 = stmt2 }
  end
end

local function foldBin(list)
  local tree = list[1]

  for i = 2, #list, 2 do
    tree = { tag = "binop", left = tree, op = list[i], right = list[i + 1] }
  end

  return tree
end

local function foldUnary(list)
  if #list > 1 then
    return { tag = "unop", op = list[1], right = list[2] }
  else
    return list[1]
  end
end

local function foldIndex(list)
  local tree = list[1]

  for i = 2, #list do
    tree = { tag = "indexed", array = tree, index = list[i] }
  end

  return tree
end

local function foldNew(list)
  return { tag = "new", dimensions = { tag = "number", val = #list } , size = list }
end

local singlelineComment = "#" * (lpeg.P(1) - "\n")^0
local multilineComment = "#{" * (lpeg.P(1) - "#}")^0 * "#}"
local comment = multilineComment + singlelineComment

local alpha = lpeg.R("AZ", "az")
local underscore = lpeg.P("_")
local digit = lpeg.R("09")
local alphanum = alpha + digit

local space = lpeg.V"space"

-- tokens like (, ), {, }, etc
local function T (t)
  return t * space
end

-- reserved words like return , if , else, etc
local reservedWords = {
  "return", "if", "elsif", "else",
  "while", "and", "or", "new"
}

local excluded = lpeg.P(false)
for i = 1, #reservedWords do
  excluded = excluded + reservedWords[i]
end
excluded = excluded * -alphanum

local function RW (t)
  if not excluded:match(t) then
    error("'"..t.."' is not a reserved word")
    os.exit(1)
  end
  -- the -alphanum is here to avoid something like return1 to be valid
  return t * -alphanum * space
end

local hex = "0" * lpeg.S("xX") * ( lpeg.R("09", "af", "AF") )^1
local decimal = lpeg.R("09")^1 + lpeg.R("19")^1 * lpeg.R("09")^1
local floating = decimal^0 * "." * decimal^1
local scientific = floating * lpeg.S("eE") * lpeg.P("-")^-1 * decimal
local numeral = ( scientific + floating  + hex + decimal ) / tonumber / node("number", "val") * space

local ID = lpeg.C(underscore^0 * alpha * alphanum^0 - excluded) * space
local var = ( ID / node("variable", "var") ) * space

local opAdd = lpeg.C(lpeg.S"+-") * space
local opMul = lpeg.C(lpeg.S"*/%") * space
local opPow = lpeg.C("^") * space
local opCmp = lpeg.C( lpeg.S("<>") * lpeg.P("=")^-1 + lpeg.P("==") + lpeg.P("!=") ) * space
local opUn  = lpeg.C("-") * space
local opNot = lpeg.C("!") * space
local opAnd = lpeg.C("and") * space
local opOr = lpeg.C("or") * space
local opLog = opAnd + opOr

local lhs = lpeg.V"lhs" -- left-hand side
local factor = lpeg.V"factor"
local term = lpeg.V"term"
local pow = lpeg.V"pow"
local expr = lpeg.V"expr"
local cmp = lpeg.V"cmp"
local minus = lpeg.V"minus"
local neg = lpeg.V"neg"

local stmt = lpeg.V"stmt"
local ifStmt = lpeg.V"ifStmt"
local whileStmt = lpeg.V"whileStmt"
local stmts = lpeg.V"stmts"
local block = lpeg.V"block"
local log = lpeg.V"log"

-- used to track max characters matched before
-- erroring out
local maxmatch = 0

grammar = lpeg.P{
  "prog",
  prog = space * stmts * -1,
  stmts = stmt * ( T";"^1 * stmts )^-1 * T";"^0 / nodeStmts, -- stmt1; stmt2; stmt3 ==> stmt1; ( stmt2; stmt3 )
  block = T"{" * stmts * T"}" + T"{" * T"}" / node("emptyBlock"),
  ifStmt = log * block * ( RW"elsif" * ifStmt + RW"else" * block )^-1 / node("if-then", "cond", "thenstmt", "elsestmt"),
  whileStmt = RW"while" * log * block / node("while-loop", "cond", "body"),
  stmt = T"@" * log / node("print", "expr") +
         block +
         RW"if" * ifStmt +
         whileStmt +
         lhs * T"=" * log / node("assignment", "lhs", "expr") +
         RW"return" * log / node("return", "expr"),
  lhs = lpeg.Ct( var * ( T"[" * log * T"]" )^0  ) / foldIndex,
  expr = RW"new" * lpeg.Ct( ( T"[" * log * T"]" )^1 ) / foldNew +
         numeral +
         T"(" * log * T")" +
         lhs,
  minus = lpeg.Ct( opUn * minus  + expr ) / foldUnary,
  pow = lpeg.Ct( minus * ( opPow * minus )^0 ) / foldBin,
  factor = lpeg.Ct( pow * ( opMul * pow )^0 ) / foldBin,
  term = lpeg.Ct( factor * ( opAdd * factor )^0 ) / foldBin,
  cmp = lpeg.Ct ( term * ( opCmp * term )^0 ) / foldBin,
  neg =  lpeg.Ct( opNot * neg  + cmp ) / foldUnary,
  log = lpeg.Ct ( neg * ( opLog * neg )^0 ) / foldBin,
  space = ( locale.space + comment )^0 * lpeg.P(
    function (_, p)
      maxmatch = math.max(p, maxmatch)

      return true
    end
  )
}

local function syntaxError(input, position)
  local lineError = select(2, string.sub(input, 1, position):gsub('\n', '\n'))
  io.stderr:write("Syntax error on line "..(lineError)..": \n")
  io.stderr:write(
    string.sub(input, position - 5, position)
    ..
    "|"
    ..
    string.sub(input, position + 1, position + 6)
  )
end

local function parse (input)
  local ast = grammar:match(input)
  if not ast then
    syntaxError(input, maxmatch)
    os.exit(1)
  end
  return ast
end


local Compiler = {
  code = {},
  vars = {},
  nvars = 0
}

function Compiler:addCode(op)
  self.code[#(self.code) + 1] = op
end

local ops = {
  ["+"] = "add",
  ["-"] = "sub",
  ["*"] = "mul",
  ["/"] = "div",
  ["%"] = "rem",
  ["^"] = "exp",
  ["<"] = "lt",
  ["<="] = "lte",
  [">"] = "gt",
  [">="] = "gte",
  ["=="] = "eq",
  ["!="] = "ne",
  ["and"] = "and",
  ["or"] = "or"
}

local unops = {
  ["-"] = "minus",
  ["!"] = "not"
}

function Compiler:var2num(id)
  local num = self.vars[id]

  if not num then
    num = self.nvars + 1
    self.nvars = num
    self.vars[id] = num
  end

  return num
end

function Compiler:jmpTo(op, label)
  self:addCode(op)
  self:addCode(label)
end

function Compiler:codeJmp(op)
  -- we don't know where the jump is jumping to
  -- so we initialize it with a zero
  self:jmpTo(op, 0)

  return self:currentPosition()
end

function Compiler:currentPosition()
  return #self.code
end

function Compiler:updateJmp(jmp)
  self.code[jmp] = self:currentPosition() - jmp
end

function Compiler:codeAssgn(ast)
  local lhs = ast.lhs

  if lhs.tag == "variable" then
    self:codeExp(ast.expr)
    self:addCode("store")
    self:addCode(self:var2num(lhs.var))
  elseif lhs.tag == "indexed" then
    self:codeExp(lhs.array)
    self:codeExp(lhs.index)
    self:codeExp(ast.expr)
    self:addCode("setarray")
  else
    error("Unknown assigment tag '".."'")
  end
end

function Compiler:codeExp(ast)
  if ast.tag == "number" then
    self:addCode("push")
    self:addCode(ast.val)
  elseif ast.tag == "binop" then
    op = ops[ast.op]

    if op == nil then
      error("Binary operation '"..ast.op.."' is not defined")
      os.exit(1)
    end

    if op == "and" then
      self:codeExp(ast.left)
      jmpRight = self:codeJmp("jmpZP")
      self:codeExp(ast.right)
      self:updateJmp(jmpRight)
    elseif op == "or" then
      self:codeExp(ast.left)
      jmpRight = self:codeJmp("jmpNZP")
      self:codeExp(ast.right)
      self:updateJmp(jmpRight)
    else
      self:codeExp(ast.left)
      self:codeExp(ast.right)
      self:addCode(op)
    end
  elseif ast.tag == "variable" then
    self:addCode("load")
    if self.vars[ast.var] == nil then
      error("variable "..ast.var.." has not been declared")
    else
      self:addCode(self:var2num(ast.var))
    end
  elseif ast.tag == "indexed" then
    self:codeExp(ast.array)
    self:codeExp(ast.index)
    self:addCode("getarray")
  elseif ast.tag == "new" then
    for i = 1, ast.dimensions.val do
      self:codeExp(ast.size[i])
    end
    self:codeExp(ast.dimensions)
    self:addCode("newarray")
  elseif ast.tag == "unop" then
    self:codeExp(ast.right)
    if unops[ast.op] == nil then
      error("Unary operation '"..ast.op.."' is not defined")
      os.exit(1)
    end
    self:addCode(unops[ast.op])
  else
    error("Invalid expression: unknown tag '"..ast.tag.."'")
  end
end

function Compiler:codeStmt(ast)
  if ast.tag == "assignment" then
    self:codeAssgn(ast)
  elseif ast.tag == "sequence" then
    self:codeStmt(ast.stmt1)
    self:codeStmt(ast.stmt2)
  elseif ast.tag == "emptyBlock" then
    -- do nothing
  elseif ast.tag == "return" then
    self:codeExp(ast.expr)
    self:addCode("return")
  elseif ast.tag == "print" then
    self:codeExp(ast.expr)
    self:addCode("print")
  elseif ast.tag == "if-then" then
    -- condcondcond
    -- condcondcond
    -- jmpX
    -- adress to skip if-cond if cond is falsy -> (%)
    --- ifififififif
    --- ifififififif
    -- jmp
    -- address to skip else-cond if cond is truthy -> codecodecode
    --- elseelse (%)
    --- elseelse
    -- codecodecode
    self:codeExp(ast.cond)
    local jmpIf = self:codeJmp("jmpX")
    self:codeStmt(ast.thenstmt)
    if ast.elsestmt == nil then
      self:updateJmp(jmpIf)
    else
      local jmpElse = self:codeJmp("jmp")
      self:updateJmp(jmpIf)
      self:codeStmt(ast.elsestmt)
      self:updateJmp(jmpElse)
    end
  elseif ast.tag == "while-loop" then
    local ilabel = self:currentPosition()
    self:codeExp(ast.cond)
    local jmp = self:codeJmp("jmpX")
    self:codeStmt(ast.body)
    self:jmpTo("jmp", ilabel - self:currentPosition() - 2) -- back to conditional
    self:updateJmp(jmp)
  else
    error("invalid statement")
  end
end

function Compiler:compile(ast)
  self:codeStmt(ast)

  -- final 'return 0' in case the program has no final return
  self:addCode("push")
  self:addCode(0)
  self:addCode("return")

  return self.code
end

local function initArray(sizes, current)
  local current = current or 1
  local array = { size = sizes[current] }
  if current < #sizes then
    for i = 1, array.size do
      array[i] = initArray(sizes, current + 1)
    end
  end

  return array
end

local function run (code, mem, stack)
  -- program counter
  local pc = 1
  local top = 0
  while true do
    if code[pc] == "return" then
      return stack[top]
    elseif code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
      print(pc..". push "..stack[top])
    elseif code[pc] == "add" then
      print(pc..". add "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] + stack[top]
      top = top - 1
    elseif code[pc] == "sub" then
      print(pc..". sub "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] - stack[top]
      top = top - 1
    elseif code[pc] == "mul" then
      print(pc..". mul "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] * stack[top]
      top = top - 1
    elseif code[pc] == "div" then
      print(pc..". div "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] / stack[top]
      top = top - 1
    elseif code[pc] == "rem" then
      print(pc..". rem "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] % stack[top]
      top = top - 1
    elseif code[pc] == "exp" then
      print(pc..". exp "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] ^ stack[top]
      top = top - 1
    elseif code[pc] == "eq" then
      print(pc..". == "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] == stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "ne" then
      print(pc..". != "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] ~= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "lt" then
      print(pc..". < "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] < stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "gt" then
      print(pc..". > "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] > stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "lte" then
      print(pc..". <= "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] <= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "gte" then
      print(pc..". >= "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] >= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "minus" then
      print(pc..". - "..stack[top])
      stack[top] = -stack[top]
    elseif code[pc] == "not" then
      print(pc..". not "..stack[top])
      stack[top] = stack[top] == 0 and 1 or 0
    elseif code[pc] == "load" then
      print(pc..". load "..code[pc+1])
      pc = pc + 1
      local id = code[pc]
      top = top + 1
      stack[top] = mem[id]
    elseif code[pc] == "store" then
      print(pc..". store "..code[pc+1])
      pc = pc + 1
      local id = code[pc]
      mem[id] = stack[top]
      top = top - 1
    elseif code[pc] == "print" then
      print(pc..". print")
      print(inspect(stack[top]))
      top = top - 1
    elseif code[pc] == "newarray" then
      local dimensions = stack[top]
      top = top - 1
      local sizes = {}
      for i = dimensions, 1, -1 do
        sizes[i] = stack[top]
        top = top - 1
      end
      top = top + 1
      stack[top] = initArray(sizes)
    elseif code[pc] == "getarray" then
      local array = stack[top - 1]
      local index = stack[top]
      if index > array.size then
        error("index out of range: "..index)
      end
      stack[top - 1] = array[index]
      top = top - 1
    elseif code[pc] == "setarray" then
      local array  = stack[top - 2]
      local index = stack[top  - 1]
      if index > array.size then
        error("index out of range: "..index)
      end
      local value = stack[top]
      array[index] = value
      top = top - 3
    elseif code[pc] == "jmpX" then -- conditional jmp
      print(pc..". jmpX "..code[pc+1])
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then
        pc = pc + code[pc] -- jump to position pc + code[pc]
      end
      top = top - 1
    elseif code[pc] == "jmp" then
      print(pc..". jmp "..(code[pc+1] - 1))
      pc = pc + 1
      pc = pc + code[pc]
      top = top - 1
    elseif code[pc] == "jmpZP" then -- and
      print(pc..". jmpZP "..(code[pc+1] - 1))
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then
        pc = pc + code[pc] -- skip if 0
      else
        top = top - 1 -- evaluate next value on the stack if != 0
      end
    elseif code[pc] == "jmpNZP" then -- or
      print(pc..". jmpNZP "..(code[pc+1] - 1))
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then
        top = top - 1 -- evaluate next value on the stack if == 0
      else
        pc = pc + code[pc] -- skip if != 0
      end
    else
      error("unknown instruction at "..pc..": "..pt(code[pc]))
    end

    pc = pc + 1
  end
end

local input = io.read("a")
local ast = parse(input)
print("AST\n\n"..pt(ast).."\n")
-- code is a tree representing the operations
local code = Compiler:compile(ast)
print("CODE\n\n"..pt(code).."\n")
-- -- stack is where the values are manipulates
local stack = {}
local mem = {}
ret = run(code, mem, stack)

print("RESULT: "..pt(mem.result))
print("RETURN: "..pt(ret))
