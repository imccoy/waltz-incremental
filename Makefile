default: B.hcr rad
	./rad

B.hcr: B.hs
	ghc -fext-core -c B.hs

rad: rad.hs
	ghc -package extcore rad.hs

clean:
	rm B.hcr B.hi B.o rad.hi rad.o rad
