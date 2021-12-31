RUNFORTH=./planck < bootstrap.fs
TESTOBJECTS=\
		../tests/return123.pko

default: planck $(TESTOBJECTS)

planck: planckforth
	make -C planckforth $(RUNTIME_IMPL)
	cp planckforth/planck .
	cp -r planckforth/lib .
	cp planckforth/bootstrap.fs .

.SUFFIXES: .pk .pko

.pk.pko:
	$(RUNFORTH) compile.fs $^ $@

.PHONY: clean

test: planck
	$(RUNFORTH) runtests.fs

clean:
	rm -rf planck lib bootstrap.fs
