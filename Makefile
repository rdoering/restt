CC := erlc

ERL_OUTDIR := ./ebin/
ERL_SRC    := $(wildcard src/*.erl)
ERL_OBJ    := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})

all: build

build: $(ERL_OBJ)

ebin/%.beam: src/%.erl
	$(CC) -pa ../ibrowse/ebin/ -pa ../proper/ebin/ -o $(ERL_OUTDIR) $<


clean:
	rm -v $(ERL_OBJ)

