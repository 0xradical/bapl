local lpeg = require "lpeg"
local inspect = require "inspect"

local locale = lpeg.locale()

local function node (num)
  return { tag = "number", val = tonumber(num) }
end


local function foldBin(list)
  local tree = list[1]

  for i = 2, #list, 2 do
    tree = { tag = "binop", left = tree, op = list[i], right = list[i + 1] }
  end

  return tree
end

local S = locale.space^0
local hex = "0" * lpeg.S("xX") * ( lpeg.R("09", "af", "AF") )^1
local decimal = lpeg.R("09")^1 + lpeg.R("19")^1 * lpeg.R("09")^1
local floating = decimal^0 * "." * decimal^1
local scientific = floating * lpeg.S("eE") * lpeg.P("-")^-1 * decimal
local numeral = ( lpeg.P("-")^-1 * ( scientific + floating  + hex + decimal ) ) / node * S
local opAdd = lpeg.C(lpeg.S"+-") * S
local opMul = lpeg.C(lpeg.S"*/%") * S
local opExp = lpeg.C("^") * S
local opCmp = lpeg.C( lpeg.S("<>") * lpeg.P("=")^-1 + lpeg.P("==") + lpeg.P("!=") ) * S
local OP = "(" * S
local CP = ")" * S

local factor = lpeg.V"factor"
local term = lpeg.V"term"
local pow = lpeg.V"pow"
local exp = lpeg.V"exp"
local cmp = lpeg.V"cmp"

grammar = S * lpeg.P{
  "cmp",
  factor = numeral + OP * cmp * CP,
  pow = lpeg.Ct( factor * ( opExp * factor )^0 ) / foldBin,
  term = lpeg.Ct( pow * ( opMul * pow )^0 ) / foldBin,
  exp = lpeg.Ct( term * ( opAdd * term )^0 ) / foldBin,
  cmp = lpeg.Ct ( exp * ( opCmp * exp )^0 ) / foldBin
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

local function codeExp(state, ast)
  if ast.tag == "number" then
    addCode(state, "push")
    addCode(state, ast.val)
  elseif ast.tag == "binop" then
    codeExp(state, ast.left)
    codeExp(state, ast.right)
    addCode(state, ops[ast.op])
  else
    error("invalid AST")
  end
end

local function compile (ast)
  local state = { code = {} }
  codeExp(state, ast)

  return state.code
end

local function run (code, stack)
  local pc = 1
  local top = 0
  while pc <= #code do
    if code[pc] == "push" then
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
    else
      error("unknown instruction")
    end

    pc = pc + 1
  end
end

local input = io.read("a")
local ast = parse(input)
print(inspect(ast))
local code = compile(ast)
print(inspect(code))
local stack = {}
run(code, stack)

print(stack[1])