RUNFORTH=./planck < bootstrap.fs

default: planck

planck: planckforth
	make $(RUNTIME_IMPL) -C planckforth
	cp planckforth/planck .
	cp -r planckforth/lib .
	cp planckforth/bootstrap.fs .

.PHONY: clean

test: planck
	$(RUNFORTH) runtests.fs

clean:
	rm -rf planck lib bootstrap.fs
