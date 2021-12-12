RUNTIME_IMPL?=i386-linux-handwritten

.PHONY: bootstrap test clean

bootstrap:
	make -C bootstrap RUNTIME_IMPL=$(RUNTIME_IMPL)

test:
	make test -C bootstrap

clean:
	make clean -C bootstrap
