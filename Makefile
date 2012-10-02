all: dependencies build docs

dependencies:
	rebar get-deps

build:
	rebar compile
docs:
	rebar doc skip_deps=true

clean:
	rebar clean

run:
	erl -pz ebin/ deps/*/ebin
