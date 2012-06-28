all: gen2.hs
	ghc --make -O2 gen2.hs -rtsopts -threaded -eventlog

check: checkPos.hs
	ghc --make -O2 checkPos.hs -rtsopts -threaded -eventlog

gen3: gen3.hs
	ghc --make -O2 gen3.hs -rtsopts -threaded -eventlog

doc:zzz.tex
	xelatex -shell-escape zzz.tex

clean:
	rm *.o *.hi


