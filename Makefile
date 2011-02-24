REBAR=./rebar

.PHONY: all clean test

all:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test:
	@$(REBAR) eunit
