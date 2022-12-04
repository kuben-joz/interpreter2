all: install

install:
	$(MAKE) -C src
	cp src/Main ./latc

.PHONY: clean
clean:
	find . -name \*.o -type f -delete
	find . -name \*.hi -type f -delete
	rm src/Main
	rm latc