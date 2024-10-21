.PHONY: all clean repl

all:
	spago build
	spago run

repl:
	erl -pa ebin

clean:
	@rm -rf output/
	@rm -rf ebin/
	@rm -rf .spago/
	@rm -rf .purs-repl
