REBAR=./rebar3
ELVIS=./elvis

build:
	$(REBAR) compile

clean:
	$(REBAR) clean

dialyzer:
	$(REBAR) dialyzer

test:
	$(REBAR) ct

elvis:
	$(ELVIS) rock

todo:
	$(REBAR) todo
