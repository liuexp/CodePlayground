SRC:=$(shell ls -A |sed -e "s/[^.]\+\.tar\.gz//g"|sed -e "s/[^.]\+\.zip//g")
BOOK=readme

all:
	xelatex --shell-escape $(BOOK).tex

present:
	xelatex present.tex
	impressive ./present.pdf
clean:
	rm -f *.aux *.toc *.lon *.lor *.lof *.ilg *.idx *.ind *.out *.log *.exa *.swp

tarball: clean
	rm -rf BEES.tar.gz
	tar -czvf BEES.tar.gz $(SRC)
zip: clean
	rm -rf BEES.zip
	zip -q -r BEES.zip $(SRC)

