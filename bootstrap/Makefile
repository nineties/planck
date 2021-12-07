RUNFORTH=./planck < bootstrap.fs

default: planck
	$(RUNFORTH) compile.fs

planck:
	make -C planckforth
	cp planckforth/planck .
	cp -r planckforth/lib .
	cp planckforth/bootstrap.fs .

.PHONY: clean

clean:
	rm -rf planck lib bootstrap.fs
