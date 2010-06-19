all:
	@./rebar compile

tests:
	@./rebar eunit

clean:
	@./rebar clean

analyze:
	@./rebar analyze
