RUNFORTH=./planck < bootstrap.fs
RUNVM=./planck < bootstrap.fs vm.fs
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
	$(RUNVM) ../tests/return123.pko; test $$? -eq 123

clean:
	rm -rf planck lib bootstrap.fs
