default: Bprime Bprime-web Bprime-web-db
	./Bprime
	echo "try Bprime-web or Bprime-web-db"

Bprime: Bprime.hs Radtime.hs Bprime.main.hs Bprime.instances.hs
	ghc -o Bprime Bprime.hs Bprime.main.hs Bprime.instances.hs

Bprime-web: Bprime.hs Radtime.hs Bprime.web.hs Bprime.instances.hs
	ghc -o Bprime-web Bprime.hs Bprime.web.hs Bprime.instances.hs

Bprime-web-db: Bprime.hs Radtime.hs Bprime.web.hs Bprime.dbinstances.hs
	ghc -o Bprime-web Bprime.hs Bprime.dbweb.hs Bprime.dbinstances.hs Bprime.dbinstances-manual.hs

Bprime.instances.hs: InMemoryApplier
	./InMemoryApplier # reads Bprime.hs and produces Bprime.instances.hs

InMemoryApplier: InMemoryApplier.hs Utils.hs
	ghc -o InMemoryApplier InMemoryApplier.hs Utils.hs

Bprime.dbinstances.hs: DatabaseApplier
	./DatabaseApplier # reads Bprime.hs and produces Bprime.dbinstances.hs

DatabaseApplier: DatabaseApplier.hs Utils.hs
	ghc -o DatabaseApplier DatabaseApplier.hs Utils.hs

web: Waltz.hs App.hs
	ghc -o App App.hs
	./App

Bprime.hs: hcr2hs Bprime.hcr
	./hcr2hs Bprime.hcr Bprime.hs
	(echo "2i"; echo "import Radtime"; echo "."; echo "wq") | ed Bprime.hs

hcr2hs: hcr2hs.hs HcrHs.hs Zcode.hs
	ghc --make hcr2hs.hs HcrHs.hs

Bprime.hcr: Incrementalizer B.hcr
	./Incrementalizer # rad is hardcoded to read B.hcr and produce Bprime.hcr

B.hcr: B.hs
	ghc -fext-core -fforce-recomp -c B.hs

Incrementalizer: Incrementalizer.hs Zcode.hs
	ghc --make -package extcore -o Incrementalizer Incrementalizer.hs IncrementalizerMain.hs

clean:
	rm *.hcr *.hi *.o Bprime.hs Bprime.hcr Bprime.instances.hs App hcr2hs Incrementalizer InMemoryApplier
