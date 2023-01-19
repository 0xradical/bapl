local lpeg = require "lpeg"
local inspect = require "inspect"

local locale = lpeg.locale()

local function nodeNum (num)
  return { tag = "number", val = tonumber(num) }
end

local function nodeVar (var)
  return { tag = "variable", var = var}
end

local function nodeAssgn (id, expr)
  return { tag = "assignment", id = id, expr = expr}
end

local function nodeEmptyBlock ()
  return { tag = "emptyBlock" }
end

local function nodeRet (expr)
  return { tag = "return", expr = expr }
end

local function nodePrint (expr)
  return { tag = "print", expr = expr }
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

local S = locale.space^0
local alpha = lpeg.R("AZ", "az")
local underscore = lpeg.P("_")
local digit = lpeg.R("09")
local alphanum = alpha + digit

local hex = "0" * lpeg.S("xX") * ( lpeg.R("09", "af", "AF") )^1
local decimal = lpeg.R("09")^1 + lpeg.R("19")^1 * lpeg.R("09")^1
local floating = decimal^0 * "." * decimal^1
local scientific = floating * lpeg.S("eE") * lpeg.P("-")^-1 * decimal
local numeral = ( scientific + floating  + hex + decimal ) / nodeNum * S

local ID = lpeg.C(underscore^0 * alpha * alphanum^0) * S
local var = ( ID / nodeVar ) * S
local Assgn = "=" * S

local opAdd = lpeg.C(lpeg.S"+-") * S
local opMul = lpeg.C(lpeg.S"*/%") * S
local opPow = lpeg.C("^") * S
local opCmp = lpeg.C( lpeg.S("<>") * lpeg.P("=")^-1 + lpeg.P("==") + lpeg.P("!=") ) * S
local opUn  = lpeg.C("-") * S

local OP  = "(" * S
local CP  = ")" * S
local OB  = "{" * S
local CB  = "}" * S
local SC  = ";" * S
local RET = "return" * S
local AT = "@" * S

local factor = lpeg.V"factor"
local term = lpeg.V"term"
local pow = lpeg.V"pow"
local expr = lpeg.V"expr"
local cmp = lpeg.V"cmp"
local un = lpeg.V"un"

local stmt = lpeg.V"stmt"
local stmts = lpeg.V"stmts"
local block = lpeg.V"block"

grammar = S * lpeg.P{
  "stmts",
  stmts = stmt * ( SC^1 * stmts )^-1 * SC^0 / nodeStmts, -- stmt1; stmt2; stmt3 ==> stmt1; ( stmt2; stmt3 )
  block = OB * stmts * CB + OB * CB / nodeEmptyBlock,
  stmt = AT * cmp / nodePrint + block + ID * Assgn * cmp / nodeAssgn + RET * cmp / nodeRet,
  expr = numeral + OP * cmp * CP + var,
  un = lpeg.Ct( opUn * un  + expr ) / foldUnary,
  pow = lpeg.Ct( un * ( opPow * un )^0 ) / foldBin,
  factor = lpeg.Ct( pow * ( opMul * pow )^0 ) / foldBin,
  term = lpeg.Ct( factor * ( opAdd * factor )^0 ) / foldBin,
  cmp = lpeg.Ct ( term * ( opCmp * term )^0 ) / foldBin
} * -1

local function parse (input)
  local ast = grammar:match(input)
  if not ast then
    error("Input doesn't match grammar")
  end
  return ast
end

local function addCode (state, op)
  local code = state.code
  code[#code + 1] = op
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
  ["!="] = "ne"
}

local unops = {
  ["-"] = "minus"
}

local function codeExp(state, ast)
  if ast.tag == "number" then
    addCode(state, "push")
    addCode(state, ast.val)
  elseif ast.tag == "binop" then
    codeExp(state, ast.left)
    codeExp(state, ast.right)
    addCode(state, ops[ast.op])
  elseif ast.tag == "variable" then
    addCode(state, "load")
    addCode(state, ast.var)
  elseif ast.tag == "unop" then
    codeExp(state, ast.right)
    addCode(state, unops[ast.op])
  else
    error("invalid expression")
  end
end

local function codeStmt(state, ast)
  if ast.tag == "assignment" then
    -- code lives at the top of the stack
    codeExp(state, ast.expr)
    addCode(state, "store")
    addCode(state, ast.id)
  elseif ast.tag == "sequence" then
    codeStmt(state, ast.stmt1)
    codeStmt(state, ast.stmt2)
  elseif ast.tag == "emptyBlock" then
    -- do nothing
  elseif ast.tag == "return" then
    codeExp(state, ast.expr)
    addCode(state, "return")
  elseif ast.tag == "print" then
    codeExp(state, ast.expr)
    addCode(state, "print")
  else
    error("invalid statement")
  end
end

local function compile (ast)
  local state = { code = {} }
  codeStmt(state, ast)

  -- final 'return 0' in case the program has no final return
  addCode(state, "push")
  addCode(state, 0)
  addCode(state, "return")

  return state.code
end

local function run (code, mem, stack)
  local pc = 1
  local top = 0
  while true do
    if code[pc] == "return" then
      return stack[top]
    elseif code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
      print("push "..stack[top])
    elseif code[pc] == "add" then
      print("add "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] + stack[top]
      top = top - 1
    elseif code[pc] == "sub" then
      print("sub "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] - stack[top]
      top = top - 1
    elseif code[pc] == "mul" then
      print("mul "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] * stack[top]
      top = top - 1
    elseif code[pc] == "div" then
      print("div "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] / stack[top]
      top = top - 1
    elseif code[pc] == "rem" then
      print("rem "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] % stack[top]
      top = top - 1
    elseif code[pc] == "exp" then
      print("exp "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] ^ stack[top]
      top = top - 1
    elseif code[pc] == "eq" then
      print("== "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] == stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "ne" then
      print("!= "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] ~= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "lt" then
      print("< "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] < stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "gt" then
      print("> "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] > stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "lte" then
      print("<= "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] <= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "gte" then
      print(">= "..stack[top - 1].." "..stack[top])
      stack[top - 1] = stack[top - 1] >= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "minus" then
      print("- "..stack[top])
      stack[top] = -stack[top]
    elseif code[pc] == "load" then
      print("load "..code[pc+1])
      pc = pc + 1
      local id = code[pc]
      top = top + 1
      stack[top] = mem[id]
    elseif code[pc] == "store" then
      print("store "..code[pc+1])
      pc = pc + 1
      local id = code[pc]
      mem[id] = stack[top]
      top = top - 1
    elseif code[pc] == "print" then
      print("print")
      print(stack[top])
      top = top - 1
    else
      error("unknown instruction "..inspect(code[pc]))
    end

    pc = pc + 1
  end
end

local input = io.read("a")
local ast = parse(input)
print(inspect(ast))
-- code is a tree representing the operations
local code = compile(ast)
print(inspect(code))
-- stack is where the values are manipulates
local stack = {}
local mem = {}
ret = run(code, mem, stack)

print("RESULT: "..inspect(mem.result))
print("RETURN: "..inspect(ret))
