VM_IMPL?=c
RUN_VM=$(VM_IMPL)/planckvm

default:
	make -C $(VM_IMPL)

.PHONY: clean test

test:
	make -C $(VM_IMPL)

test-vm:
	make -C $(VM_IMPL)
	$(RUN_VM) ../tests/return123.pko; test $$? -eq 123
	$(RUN_VM) ../tests/goto.pko; test $$? -eq 123
	$(RUN_VM) ../tests/simple-assign.pko; test $$? -eq 123
	$(RUN_VM) ../tests/simple-arithmetic.pko; test $$? -eq 123

clean:
	make -C $(VM_IMPL)
