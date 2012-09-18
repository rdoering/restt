CC := erlc

ERL_OUTDIR := ./ebin/
ERL_SRC    := $(wildcard src/*.erl)
ERL_OBJ    := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})
all: build

build: $(ERL_OBJ)

ebin/%.beam: src/%.erl
	$(CC) -o $(ERL_OUTDIR) $<


clean:
	rm -v $(ERL_OBJ)

run:
	cd src && erl 

