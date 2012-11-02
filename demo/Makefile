Incrementaliser=../incrm/dist/build/Incrementaliser/Incrementaliser
dbrun=../dbrun/dist/build/dbrun/dbrun

default: Bmain Bweb
	./Bmain
	$(dbrun)
	echo "try Bweb for moar fun"

B.hi: B.hs $(Incrementaliser) InctimeHtml.hi
	$(Incrementaliser) B && as B.s -o B.o

InctimeHtml.hi: InctimeHtml.hs $(Incrementaliser)
	$(Incrementaliser) InctimeHtml && as InctimeHtml.s -o InctimeHtml.o

Bmain: B.hi Inctime.hs Bmain.hs
	ghc --make Bmain.hs -fext-core

Bweb: BwebMain.hs B.hi InctimeHtml.hi InctimeWeb.hs
	ghc --make -o Bweb BwebMain.hs -fext-core

clean:
	rm -f *.hcr *.hi *.o Bmain Bweb B.s B.js \
	     	InctimeHtml.js InctimeHtml.s InctimeUtils.js InctimeWeb.js
