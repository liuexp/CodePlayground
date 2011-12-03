SRC:=$(shell ls -A |sed -e "s/[^.]\+\.tar\.gz//g"|sed -e "s/[^.]\+\.zip//g")
all:
	ghc  -XExistentialQuantification -o main main.hs
clean:
	rm -rf *.hi *.o

tarball: clean
	rm -rf Schemer.tar.gz
	tar -czvf Schemer.tar.gz $(SRC)
zip: clean
	rm -rf Schemer.zip
	zip -q -r Schemer.zip $(SRC)

