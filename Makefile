all: install

install:
	$(MAKE) -C src/haskellsrc
	$(MAKE) -C src/csrc/build
	cp src/latc ./latc

.PHONY: clean
clean:
	find . -name \*.o -type f -delete
	find . -name \*.hi -type f -delete
	rm src/Main
	rm src/csrc/build/
	rm latc