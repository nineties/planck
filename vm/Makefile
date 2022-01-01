VM_IMPL?=c
RUN_VM=$(VM_IMPL)/planckvm

default:
	make -C $(VM_IMPL)

.PHONY: clean test

test:
	make -C $(VM_IMPL)

test-vm:
	$(RUN_VM) ../tests/return123.pko; test $$? -eq 123

clean:
	make -C $(VM_IMPL)
