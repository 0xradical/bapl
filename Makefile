.PHONY: run roberto

run:
	lua -l lpeg interpreter.lua < input.prog

debug:
	lua -l lpeg -l pt