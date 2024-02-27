
NULL =

all:
	cabal build

dist:
	cabal sdist

sync:
	make -C html
	scp html/*.* $(DEST)
	scp dist/*.tar.gz $(DEST)

.PHONY: dist clean check

clean:
	cabal clean
	rm -f EasyJoin
