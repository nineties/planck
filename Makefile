.PHONY: bootstrap clean

bootstrap:
	make -C bootstrap

clean:
	make clean -C bootstrap
