all: dependencies build docs

dependencies:
	./rebar get-deps

build:
	./rebar compile
docs:
	./rebar doc skip_deps=true

clean:
	./rebar clean

run:
	erl -pz ebin/ deps/*/ebin

run-insrc:
	cd src;	erl -pz ../deps/*/ebin

wrun:
	werl -pz ebin/ deps/*/ebin

wrun-insrc:
	cd src;	werl -pz ../deps/*/ebin
