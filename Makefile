default: Bprime.hs B.hcr rad hcr2hs
	ghc -o Bprime Bprime.hs Bprime.main.hs
	./Bprime

web: Waltz.hs App.hs
	ghc -o App App.hs
	./App

Bprime.hs: hcr2hs Bprime.hcr
	./hcr2hs # hcr2hs is hardcoded to read Bprime.hcr and produce Bprime.hs

hcr2hs: hcr2hs.hs Zcode.hs
	ghc --make hcr2hs.hs

Bprime.hcr: rad B.hcr
	./rad # rad is hardcoded to read B.hcr and produce Bprime.hcr

B.hcr: B.hs
	ghc -fext-core -c B.hs

rad: rad.hs Zcode.hs
	ghc --make -package extcore rad.hs

clean:
	rm *.hcr *.hi *.o rad Bprime.hs Bprime.hcr App hcr2hs
