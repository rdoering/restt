all: dependencies build

dependencies:
	rebar get-deps

build:
	rebar compile

clean:
	rebar clear

run:
	erl -pz ebin/ deps/*/ebin