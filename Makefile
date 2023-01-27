.PHONY: run roberto

run:
	lua -l lpeg interpreter.lua < input.prog

roberto:
	lua -l lpeg roberto/interpreter.lua < input.prog