PLANCK_IMPL?=i386-linux-handwritten
VM_IMPL?=planckforth

.PHONY: bootstrap test clean

bootstrap:
	make -C bootstrap RUNTIME_IMPL=$(PLANCK_IMPL)

test: bootstrap
ifeq ($(VM_IMPL), planckforth)
	make test -C bootstrap
else
	make test -C vm VM_IMPL=$(VM_IMPL)
endif

clean:
	make clean -C bootstrap
