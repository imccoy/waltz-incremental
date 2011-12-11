default: Bprime.hs B.hcr rad hcr2hs
	ghc -c Bprime.hs

Bprime.hs: hcr2hs Bprime.hcr
	./hcr2hs
	cat Bprime.hs

hcr2hs: hcr2hs.hs
	ghc hcr2hs.hs

Bprime.hcr: rad B.hcr
	./rad

B.hcr: B.hs
	ghc -fext-core -c B.hs

rad: rad.hs
	ghc -package extcore rad.hs

clean:
	rm B.hcr B.hi B.o rad.hi rad.o rad
