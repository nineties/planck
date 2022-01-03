RUN_FORTH=./planck < bootstrap.fs
RUN_VM=./planck < bootstrap.fs vm.fs
TESTOBJECTS=\
		../tests/return123.pko \
		../tests/goto.pko \
		../tests/simple-assign.pko

default: planck $(TESTOBJECTS)

planck: planckforth
	make -C planckforth $(RUNTIME_IMPL)
	cp planckforth/planck .
	cp -r planckforth/lib .
	cp planckforth/bootstrap.fs .

.SUFFIXES: .pk .pko

.pk.pko:
	$(RUN_FORTH) compile.fs $^ $@

.PHONY: clean

test: planck
	$(RUN_FORTH) runtests.fs

test-vm: planck $(TESTOBJECTS)
	$(RUN_VM) ../tests/return123.pko; test $$? -eq 123
	$(RUN_VM) ../tests/goto.pko; test $$? -eq 123
	$(RUN_VM) ../tests/simple-assign.pko; test $$? -eq 123

clean:
	rm -rf planck lib bootstrap.fs
