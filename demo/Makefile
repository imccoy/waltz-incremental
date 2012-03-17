bindir=../dist/build/
InMemoryApplier=$(bindir)/InMemoryApplier/InMemoryApplier
DatabaseApplier=$(bindir)/DatabaseApplier/DatabaseApplier
Incrementalizer=$(bindir)/Incrementalizer/Incrementalizer
hcr2hs=$(bindir)/hcr2hs/hcr2hs
ghcjs=../../ghcjs/dist/build/ghcjs/ghcjs

default: Bprime Bprime-web # Bprime-web-db
	./Bprime
	echo "try Bprime-web or Bprime-web-db for moar fun"

Bprime: Bprime.hs ../support/Radtime.hs Bprime.main.hs BprimeInstances.hs InctimeHtmlPrime.hs
	ghc -o Bprime Bprime.hs Bprime.main.hs BprimeInstances.hs ../support/Radtime.hs InctimeHtmlPrime.hs

Bprime-web: Bprime.hs Bprime.js Bprime.web.js ../support/Radtime.hs Bprime.web.hs BPrimeWebMain.hs BprimeInstances.hs
	ghc -o Bprime-web -main-is BPrimeWebMain Bprime.hs BPrimeWebMain.hs Bprime.web.hs BprimeInstances.hs ../support/Radtime.hs

Bprime-web-db: Bprime.hs ../support/Radtime.hs ../support/DbRadtime.hs Bprime.web.hs Bprime.dbinstances.hs
	ghc -o Bprime-web-db Bprime.hs Bprime.dbweb.hs Bprime.dbinstances.hs Bprime.dbinstances-manual.hs ../support/Radtime.hs ../support/DbRadtime.hs

BprimeInstances.hs: $(InMemoryApplier)
	$(InMemoryApplier) B.hcr BprimeInstances.hs

Bprime.dbinstances.hs: $(DatabaseApplier)
	$(DatabaseApplier) B.hcr Bprime.dbinstances.hs

Bprime.hs: $(hcr2hs) Bprime.hcr
	$(hcr2hs) Bprime.hcr Bprime.hs
	(echo "2i"; echo "import Radtime\nimport InctimeHtmlPrime"; echo "."; echo "wq") | ed Bprime.hs

Bprime.js: Bprime.hs
	$(ghcjs) Bprime.hs

Bprime.web.js: Bprime.web.hs BprimeInstances.hs Bprime.hs InctimeUtils.hs
	$(ghcjs) Bprime.hs Bprime.web.hs BprimeInstances.hs InctimeUtils.hs InctimeHtmlPrime.hs Radtime.hs

Bprime.hcr: $(Incrementalizer) B.hcr
	$(Incrementalizer) B.hcr Bprime.hcr

B.hcr: B.hs InctimeHtmlPrime.hs
	ghc -fext-core -fforce-recomp -c B.hs InctimeHtmlPrime.hs

InctimeHtmlPrime.hs: InctimeHtml.hs
	ghc -fext-core -c Radtime.hs
	ghc -fext-core -fforce-recomp -c InctimeHtml.hs
	$(Incrementalizer) InctimeHtml.hcr InctimeHtmlPrime.hcr
	$(hcr2hs) InctimeHtmlPrime.hcr InctimeHtmlPrime.hs
	rm InctimeHtml.hi InctimeHtml.o
	(echo "2i"; echo "import Radtime"; echo "."; echo "wq") | ed InctimeHtmlPrime.hs
	ghc -c InctimeHtmlPrime.hs

InctimeHtmlPrime.js: InctimeHtmlPrime.hs
	$(ghcjs) InctimeHtmlPrime.hs

clean:
	rm -f *.hcr *.hi *.o \
	   	  Bprime.hs BprimeInstances.hs Bprime.dbinstances.hs \
	      Bprime Bprime-web Bprime-web-db db \
			  B.js Bprime.js Bprime.web.js BprimeInstances.js \
				InctimeWeb.js InctimeHtml.js Radtime.js \
				Inctime*Prime.hs
