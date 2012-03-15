bindir=../dist/build/
InMemoryApplier=$(bindir)/InMemoryApplier/InMemoryApplier
DatabaseApplier=$(bindir)/DatabaseApplier/DatabaseApplier
Incrementalizer=$(bindir)/Incrementalizer/Incrementalizer
hcr2hs=$(bindir)/hcr2hs/hcr2hs

default: Bprime Bprime-web Bprime-web-db
	./Bprime
	echo "try Bprime-web or Bprime-web-db for moar fun"

Bprime: Bprime.hs ../support/Radtime.hs Bprime.main.hs Bprime.instances.hs
	ghc -o Bprime Bprime.hs Bprime.main.hs Bprime.instances.hs ../support/Radtime.hs

Bprime-web: Bprime.hs ../support/Radtime.hs Bprime.web.hs Bprime.instances.hs
	ghc -o Bprime-web Bprime.hs Bprime.web.hs Bprime.instances.hs ../support/Radtime.hs

Bprime-web-db: Bprime.hs ../support/Radtime.hs ../support/DbRadtime.hs Bprime.web.hs Bprime.dbinstances.hs
	ghc -o Bprime-web Bprime.hs Bprime.dbweb.hs Bprime.dbinstances.hs Bprime.dbinstances-manual.hs ../support/Radtime.hs ../support/DbRadtime.hs

Bprime.instances.hs: $(InMemoryApplier)
	$(InMemoryApplier) B.hcr Bprime.instances.hs

Bprime.dbinstances.hs: $(DatabaseApplier)
	$(DatabaseApplier) B.hcr Bprime.dbinstances.hs

Bprime.hs: $(hcr2hs) Bprime.hcr
	$(hcr2hs) Bprime.hcr Bprime.hs
	(echo "2i"; echo "import ../support/Radtime"; echo "."; echo "wq") | ed Bprime.hs

Bprime.hcr: $(Incrementalizer) B.hcr
	$(Incrementalizer) B.hcr Bprime.hcr

B.hcr: B.hs
	ghc -fext-core -fforce-recomp -c B.hs


clean:
	rm -f *.hcr *.hi *.o Bprime.hs Bprime.instances.hs Bprime.dbinstances.hs Bprime Bprime-web Bprime-web-db db
