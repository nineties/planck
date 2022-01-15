RUN_FORTH=./planck < bootstrap.fs
RUN_VM=./planck < bootstrap.fs vm.fs
COMPILER_SOURCES=\
	graph.fs \
	encoding.fs \
	lexer.fs \
	parser.fs \
	compile.fs
TEST_OBJECTS=\
	../tests/return123.pko \
	../tests/goto.pko \
	../tests/assign.pko \
	../tests/uint-arithmetic.pko \
	../tests/call.pko \
	../tests/call2.pko \
	../tests/call-with-args.pko \
	../tests/iftrue.pko \
	../tests/iffalse.pko \
	../tests/phi.pko \
	../tests/phi2.pko \
	../tests/backjump.pko \
	../tests/comparison.pko \
	../tests/ifcomp.pko \
	../tests/loop.pko \
	../tests/recursive.pko \
	../tests/fib.pko \
	../tests/tuple.pko \
	../tests/tuple2.pko \
	../tests/tuple3.pko \
	../tests/global-constant.pko \
	../tests/lib.pko \
	../tests/ecall.pko \
	../tests/eload.pko \
	../tests/estore.pko \
	../tests/a/b.pko \
	../tests/a/c.pko \
	../tests/ecall2.pko \
	../tests/eload2.pko \
	../tests/estore2.pko \
	../tests/relimport.pko

default: planck $(TEST_OBJECTS)

planck: planckforth
	make -C planckforth $(RUNTIME_IMPL)
	cp planckforth/planck .
	cp -r planckforth/lib .
	cp planckforth/bootstrap.fs .

.SUFFIXES: .pk .pko

%.pko: %.pk $(COMPILER_SOURCES)
	$(RUN_FORTH) compile.fs $< $@

.PHONY: clean

test: planck
	$(RUN_FORTH) runtests.fs

test-vm: planck $(TEST_OBJECTS)
	$(RUN_VM) ../tests/return123.pko; test $$? -eq 123
	$(RUN_VM) ../tests/goto.pko; test $$? -eq 123
	$(RUN_VM) ../tests/assign.pko; test $$? -eq 123
	$(RUN_VM) ../tests/uint-arithmetic.pko; test $$? -eq 123
	$(RUN_VM) ../tests/call.pko; test $$? -eq 123
	$(RUN_VM) ../tests/call2.pko; test $$? -eq 123
	$(RUN_VM) ../tests/call-with-args.pko; test $$? -eq 123
	$(RUN_VM) ../tests/iftrue.pko; test $$? -eq 123
	$(RUN_VM) ../tests/iffalse.pko; test $$? -eq 123
	$(RUN_VM) ../tests/phi.pko; test $$? -eq 123
	$(RUN_VM) ../tests/phi2.pko; test $$? -eq 123
	$(RUN_VM) ../tests/backjump.pko; test $$? -eq 123
	$(RUN_VM) ../tests/comparison.pko; test $$? -eq 123
	$(RUN_VM) ../tests/ifcomp.pko; test $$? -eq 123
	$(RUN_VM) ../tests/loop.pko; test $$? -eq 123
	$(RUN_VM) ../tests/recursive.pko; test $$? -eq 123
	$(RUN_VM) ../tests/fib.pko; test $$? -eq 55
	$(RUN_VM) ../tests/tuple.pko; test $$? -eq 123
	$(RUN_VM) ../tests/tuple2.pko; test $$? -eq 123
	$(RUN_VM) ../tests/tuple3.pko; test $$? -eq 123
	$(RUN_VM) ../tests/global-constant.pko; test $$? -eq 123
	$(RUN_VM) ../tests/ecall.pko; test $$? -eq 123
	$(RUN_VM) ../tests/eload.pko; test $$? -eq 123
	$(RUN_VM) ../tests/estore.pko; test $$? -eq 123
	$(RUN_VM) ../tests/ecall2.pko; test $$? -eq 123
	$(RUN_VM) ../tests/eload2.pko; test $$? -eq 123
	$(RUN_VM) ../tests/estore2.pko; test $$? -eq 123
	$(RUN_VM) ../tests/relimport.pko; test $$? -eq 123

clean:
	rm -rf planck lib bootstrap.fs
