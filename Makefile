PLANCK_IMPL?=i386-linux-handwritten
VM_IMPL?=planckforth

.PHONY: bootstrap test clean

bootstrap:
	make -C bootstrap RUNTIME_IMPL=$(PLANCK_IMPL)

test:
	make test -C bootstrap
ifneq ($(VM_IMPL), planckforth)
	make test -C vm VM_IMPL=$(VM_IMPL)
endif

clean:
	make clean -C bootstrap
