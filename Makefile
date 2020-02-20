REBAR=./rebar3
ELVIS=./elvis

.PHONY: build clean dialyzer test elvis todo

build:
	$(REBAR) compile

clean:
	$(REBAR) clean

dialyzer:
	$(REBAR) dialyzer

test:
	$(REBAR) proper --max_size 5

elvis:
	$(ELVIS) rock

todo:
	$(REBAR) todo
