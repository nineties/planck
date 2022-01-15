RUN_VM=./planckvm

default: planckvm

planckvm: planckvm.c
	gcc -Wall -O2 $< -o $@

.PHONY: clean test-vm

test-vm: planckvm
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
	$(RUN_VM) ../tests/import-guard.pko; test $$? -eq 123

clean:
	rm -f *.o planckvm

