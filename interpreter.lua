local lpeg = require "lpeg"
local pt = require "pt"
local inspect = require "inspect"

local locale = lpeg.locale()

-- match-time capture based debugger
local function I (msg)
  return lpeg.P(function () print(msg); return true end)
end

local function node (tag, ...)
  local labels = table.pack(...)

  return function (...)
    local params = table.pack(...)
    local t = { tag = tag }
    for i = 1, #labels do
      t[labels[i]] = params[i]
    end

    return t
  end
end

-- stmt2 is optional
local function nodeStmts (stmt1, stmt2)
  if stmt2 == nil then
    return stmt1
  else
    return { tag = "stmts", stmt1 = stmt1, stmt2 = stmt2 }
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
    tree = { tag = "indexed", var = tree.var, array = tree, index = list[i] }
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
  "while", "and", "or", "new",
  "function", "var"
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

local ID = lpeg.V"ID"
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
local funcDecl = lpeg.V"funcDecl"
local call = lpeg.V"call"
local params = lpeg.V"params" -- function definition
local args = lpeg.V"args" -- function call

-- used to track max characters matched before
-- erroring out
local maxmatch = 0

grammar = lpeg.P{
  "prog",
  -- prog = space * stmts * -1,
  prog = space * lpeg.Ct( funcDecl^1 ) * -1,
  funcDecl = RW"function" * ID * ( T";" + T"(" * params * T")" * block ) / node("function", "name", "params", "body"),
  params = lpeg.Ct( ( ID * ( T"," * ID )^0)^-1  ),
  stmts = stmt * ( T";"^1 * stmts )^-1 * T";"^0 / nodeStmts, -- stmt1; stmt2; stmt3 ==> stmt1; ( stmt2; stmt3 )
  block = T"{" * T"}" / node("block", "body") + T"{" * stmts * T"}" / node("block", "body"),
  ifStmt = log * block * ( RW"elsif" * ifStmt + RW"else" * block )^-1 / node("if-then", "cond", "thenstmt", "elsestmt"),
  whileStmt = RW"while" * log * block / node("while-loop", "cond", "body"),
  stmt = T"@" * log / node("print", "expr") +
         block +
         RW"var" * ID *  ( T"=" * log )^0 / node("local", "name", "init") + -- var is used for "local" variables
         RW"if" * ifStmt +
         whileStmt +
         call +
         lhs * T"=" * log / node("assignment", "lhs", "expr") +
         RW"return" * log / node("return", "expr"),
  lhs = lpeg.Ct( var * ( T"[" * log * T"]" )^0  ) / foldIndex,
  call = ID * T"(" * args * T")" / node("call", "fname", "args"),
  args = lpeg.Ct( ( log * ( T"," * log )^0)^-1  ),
  expr = RW"new" * lpeg.Ct( ( T"[" * log * T"]" )^1 ) / foldNew +
         numeral +
         T"(" * log * T")" +
         call +
         lhs,
  minus = lpeg.Ct( opUn * minus  + expr ) / foldUnary,
  pow = lpeg.Ct( minus * ( opPow * minus )^0 ) / foldBin,
  factor = lpeg.Ct( pow * ( opMul * pow )^0 ) / foldBin,
  term = lpeg.Ct( factor * ( opAdd * factor )^0 ) / foldBin,
  cmp = lpeg.Ct ( term * ( opCmp * term )^0 ) / foldBin,
  neg =  lpeg.Ct( opNot * neg  + cmp ) / foldUnary,
  log = lpeg.Ct ( neg * ( opLog * neg )^0 ) / foldBin,
  ID = lpeg.C(underscore^0 * alpha * alphanum^0 - excluded) * space,
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
  -- code = {},
  funcs = {},
  vars = {},
  locals = {}, -- list of active locals, naturally empty at the beginning of a function call
  nvars = 0,
  currentBlock = {},
  currentFn = {}
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

function Compiler:findLocal(name)
  local loc = self.locals
  for i = #loc, 1, -1 do
    if name == loc[i] then
      return i
    end
  end

  return nil
end

function Compiler:currentPosition()
  return #self.code
end

function Compiler:updateJmp(jmp)
  self.code[jmp] = self:currentPosition() - jmp
end

function Compiler:codeAssgn(ast)
  local lhs = ast.lhs

  if self.funcs[lhs.var] then
    error("Can't use '"..lhs.var.."' as a variable name, function exists")
  end

  if lhs.tag == "variable" then
    self:codeExp(ast.expr)

    local idx = self:findLocal(lhs.var)

    if idx then
      self:addCode("storeL")
      self:addCode(idx)
    else
      -- store as a global variable
      self:addCode("store")
      self:addCode(self:var2num(lhs.var))
    end
  elseif lhs.tag == "indexed" then
    self:codeExp(lhs.array)
    self:codeExp(lhs.index)
    self:codeExp(ast.expr)
    self:addCode("setarray")
  else
    error("Unknown assigment tag '".."'")
  end
end

function Compiler:codeBlock(ast)
  self.currentBlock = ast
  self.currentBlock.vars = self.currentBlock.vars or {}

  if ast.body ~= '{}' then
    local localsBefore = #self.locals
    self:codeStmt(ast.body)
    local localsAfter = #self.locals
    local diff = localsAfter - localsBefore
    if diff > 0 then
      for i = 1, diff do
        table.remove(self.locals)
      end

      -- pop the extra variables, i.e.,
      -- move the stack pointer to
      -- where it was
      self:addCode("pop")
      self:addCode(diff)
    end
  end -- else it's an empty block
end

function Compiler:codeCall(ast)
  local func = self.funcs[ast.fname]

  if not func then
    error("Undefined function '"..ast.fname.."'")
  end

  if #func.params ~= #ast.args then
    error("Wrong number of arguments for '"..ast.fname.."', expected "..#func.params.." arguments, got "..#ast.args)
  end

  self:addCode("call")
  self:addCode(func.code)
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
    local idx = self:findLocal(ast.var)

    if idx then
      self:addCode("loadL")
      self:addCode(idx)
    else
      if self.vars[ast.var] == nil then
        error("variable "..ast.var.." has not been declared")
      else
        self:addCode("load")
        self:addCode(self:var2num(ast.var))
      end
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
  elseif ast.tag == "call" then
    self:codeCall(ast)
  else
    error("Invalid expression: unknown tag '"..ast.tag.."'")
  end
end

function Compiler:codeStmt(ast)
  if ast.tag == "assignment" then
    self:codeAssgn(ast)
  elseif ast.tag == "stmts" then
    self:codeStmt(ast.stmt1)
    self:codeStmt(ast.stmt2)
  elseif ast.tag == "block" then
    self:codeBlock(ast)
  elseif ast.tag == "return" then
    self:codeExp(ast.expr)
    self:addCode("return")
    self:addCode(#self.locals)
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
  elseif ast.tag == "call" then
    self:codeCall(ast)
    self:addCode("pop")
    self:addCode(1)
  elseif ast.tag == "local" then
    if self.currentBlock.vars[ast.name] then
      error("Local variable named '"..ast.name.."' has already been declared")
    elseif self.currentFn then
      -- look for variable in fn params
      for i = 1, #self.currentFn.params do
        if self.currentFn.params[i] == ast.name then
          error("Local variable '"..ast.name.."' cannot be used, a parameter with that name exists in function '"..self.currentFn.name.."'")
        end
      end

      self.currentBlock.vars[ast.name] = true
    else
      self.currentBlock.vars[ast.name] = true
    end

    if ast.init then
      self:codeExp(ast.init)
    else
      self:addCode("push")
      self:addCode(0)
    end
    self.locals[#self.locals + 1] = ast.name
  else
    error("Invalid statement: unknown tag '"..ast.tag.."'")
  end
end

function Compiler:codeFunction(ast)
  if ast.name == "main" and #ast.params > 0 then
    error("Function 'main' can't have any parameters")
  end

  local fn = self.funcs[ast.name]
  local code = {}

  -- forward declaration setup
  if fn then
    code = fn.code
  else
    fn = { code = code, defined = false }
    self.funcs[ast.name] = fn
  end

  if fn.defined then
    error("function '"..ast.name.."' is defined more than once")
  elseif ast.body then -- if it has a body then define function
    fn.defined = true
    fn.params = ast.params
    self.code = code
    self.currentFn = ast
    self:codeStmt(ast.body)
    self.currentFn = {}
      -- final 'return 0' in case the function has no final return
    self:addCode("push")
    self:addCode(0)
    self:addCode("return")
    self:addCode(#self.locals)
  else
    if ast.name == "main" then
      error("Function 'main' can't be forward declared")
    end
  end
end

function compile(ast)
  local j = 0
  -- compile all functions before main
  for i = 1, #ast do
    if ast[i].name ~= "main" then
      Compiler:codeFunction(ast[i])
    else
      j = i
    end
  end

  -- then compile 'main'
  -- the order matters because
  -- the compiler registers the numbers of
  -- local variables in all functions after "return"
  -- if a function comes after main, then the number
  -- might be incorrect
  if j > 0 then
    Compiler:codeFunction(ast[j])
  else
    error("function 'main' not found")
  end

  return Compiler.funcs['main'].code
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

local function run (code, mem, stack, top)
  -- program counter
  local pc = 1
  -- the relative index 1 for the current "run" call
  -- used to calculate the position of a local var
  local base = top
  while true do
    ---[[
      io.write("--> ")
      for i = 1, top do io.write(pt(stack[i]), " ") end
      io.write("\n", code[pc], "\n")
      --]]
    if code[pc] == "return" then
      -- first remove all locals
      local n = code[pc + 1] -- number of local variables
      stack[top - n] = stack[top]
      return top - n
    elseif code[pc] == "call" then
      pc = pc + 1
      local code = code[pc]
      top = run(code, mem, stack, top)
    elseif code[pc] == "pop" then
      pc = pc + 1
      top = top - code[pc]
    elseif code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
    elseif code[pc] == "add" then
      stack[top - 1] = stack[top - 1] + stack[top]
      top = top - 1
    elseif code[pc] == "sub" then
      stack[top - 1] = stack[top - 1] - stack[top]
      top = top - 1
    elseif code[pc] == "mul" then
      stack[top - 1] = stack[top - 1] * stack[top]
      top = top - 1
    elseif code[pc] == "div" then
      stack[top - 1] = stack[top - 1] / stack[top]
      top = top - 1
    elseif code[pc] == "rem" then
      stack[top - 1] = stack[top - 1] % stack[top]
      top = top - 1
    elseif code[pc] == "exp" then
      stack[top - 1] = stack[top - 1] ^ stack[top]
      top = top - 1
    elseif code[pc] == "eq" then
      stack[top - 1] = stack[top - 1] == stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "ne" then
      stack[top - 1] = stack[top - 1] ~= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "lt" then
      stack[top - 1] = stack[top - 1] < stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "gt" then
      stack[top - 1] = stack[top - 1] > stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "lte" then
      stack[top - 1] = stack[top - 1] <= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "gte" then
      stack[top - 1] = stack[top - 1] >= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "minus" then
      stack[top] = -stack[top]
    elseif code[pc] == "not" then
      stack[top] = stack[top] == 0 and 1 or 0
    elseif code[pc] == "load" then
      pc = pc + 1
      local id = code[pc]
      top = top + 1
      stack[top] = mem[id]
    elseif code[pc] == "loadL" then
      pc = pc + 1
      local idx = code[pc]
      top = top + 1
      stack[top] = stack[base + idx]
    elseif code[pc] == "store" then
      pc = pc + 1
      local id = code[pc]
      mem[id] = stack[top]
      top = top - 1
    elseif code[pc] == "storeL" then
      pc = pc + 1
      local idx = code[pc]
      stack[base + idx] = stack[top]
      top = top - 1
    elseif code[pc] == "print" then
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
      print(inspect(stack).." @ "..top)
      local array  = stack[top - 2]
      local index = stack[top  - 1]
      if index > array.size then
        error("index out of range: "..index)
      end
      local value = stack[top]
      array[index] = value
      top = top - 3
    elseif code[pc] == "jmpX" then -- conditional jmp
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then
        pc = pc + code[pc] -- jump to position pc + code[pc]
      end
      top = top - 1
    elseif code[pc] == "jmp" then
      pc = pc + 1
      pc = pc + code[pc]
      top = top - 1
    elseif code[pc] == "jmpZP" then -- and
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then
        pc = pc + code[pc] -- skip if 0
      else
        top = top - 1 -- evaluate next value on the stack if != 0
      end
    elseif code[pc] == "jmpNZP" then -- or
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
local code = compile(ast)
print("CODE\n\n"..pt(code).."\n")
-- -- stack is where the values are manipulates
local stack = {}
local mem = {}
ret = run(code, mem, stack, 0)
io.write("--> ")
for i = 1, ret do io.write(stack[i], " ") end
io.write("\n\n")
print("Variables: "..inspect(mem.result))
print("Top: "..ret)
print("Return: "..pt(stack[ret]))