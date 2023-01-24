inspect = require("inspect")

local function node(tag, ...)
  local labels = table.pack(...)
  local params = table.concat(labels, ", ")
  local fields = string.gsub(params, "(%w+)", "%1 = %1")

  local code = string.format(
    "return function (%s) return { tag = \"%s\", %s } end",
    params, tag, fields
  )

  -- print(code)
  return load(code)()
end

f = node("assignment", "id", "expr")
print(inspect(f()))