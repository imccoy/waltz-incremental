bindir=../dist/build/
Incrementaliser=$(bindir)/Incrementaliser/Incrementaliser
ghcjs=../../ghcjs/dist/build/ghcjs/ghcjs

default: Bmain Bweb
	./Bmain
	echo "try Bweb for moar fun"

B.hi: B.hs $(Incrementaliser) InctimeHtml.hi
	$(Incrementaliser) B && as B.s -o B.o

InctimeHtml.hi: InctimeHtml.hs $(Incrementaliser)
	$(Incrementaliser) InctimeHtml && as InctimeHtml.s -o InctimeHtml.o

Bmain: B.hi Inctime.hs Bmain.hs
	ghc --make Bmain.hs -fext-core

Bweb: Bweb.js BwebMain.hs B.hi \
    	Inctime.js InctimeHtml.hi InctimeWeb.hs InctimeUtils.js ToJsonString.hs
	ghc --make -o Bweb BwebMain.hs -fext-core

Bweb.js: Bweb.hs
	$(ghcjs) Bweb.hs

Inctime.js: Inctime.hs
	$(ghcjs) Inctime.hs

InctimeUtils.js: InctimeUtils.hs
	$(ghcjs) InctimeUtils.hs

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
	rm -f *.hcr *.hi *.o Bmain Bweb B.s B.js \
	     	InctimeHtml.js InctimeHtml.s InctimeUtils.js InctimeWeb.js \
				Bweb.js Inctime.js ToJsonString.js
