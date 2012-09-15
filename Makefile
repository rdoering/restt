CC := erlc

ERL_OUTDIR := ./ebin/
ERL_SRC    := $(wildcard src/*.erl)
ERL_OBJ    := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})
ERL_LIBS   := -pz ../ibrowse/ebin/ -pz ../proper/ebin/ 
all: build

build: $(ERL_OBJ)

ebin/%.beam: src/%.erl
	$(CC) $(ERL_LIBS) -o $(ERL_OUTDIR) $<


clean:
	rm -v $(ERL_OBJ)

run:
	cd src && erl -pz ../../ibrowse/ebin/ -pz ../../proper/ebin/

