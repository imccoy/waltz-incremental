bindir=../dist/build/
Incrementaliser=$(bindir)/Incrementaliser/Incrementaliser

default: Bmain Bweb
	./Bmain
	echo "try Bweb or Bprime-web-db for moar fun"

B.hi: B.hs $(Incrementaliser)
	$(Incrementaliser) B && as B.s -o B.o

Bmain: B.hi Inctime.hs Bmain.hs
	ghc --make Bmain.hs

Bweb: B.hs Inctime.hs Bweb.hs
	ghc -o Bweb B.hs Bweb.hs Inctime.hs

Bprime-web-db: Bprime.hs ../support/Radtime.hs ../support/DbRadtime.hs Bprime.web.hs Bprime.dbinstances.hs
	ghc -o Bprime-web Bprime.hs Bprime.dbweb.hs Bprime.dbinstances.hs Bprime.dbinstances-manual.hs ../support/Radtime.hs ../support/DbRadtime.hs

Bprime.hs: $(hcr2hs) Bprime.hcr
	$(hcr2hs) Bprime.hcr Bprime.hs
	(echo "2i"; echo "import Radtime"; echo "."; echo "wq") | ed Bprime.hs

Bprime.hcr: $(Incrementalizer) B.hcr
	$(Incrementalizer) B.hcr Bprime.hcr

B.hcr: B.hs
	ghc -fext-core -fforce-recomp -c B.hs


clean:
	rm -f *.hcr *.hi *.o Bprime.hs Bprime.instances.hs Bprime.dbinstances.hs Bprime Bprime-web Bprime-web-db db
