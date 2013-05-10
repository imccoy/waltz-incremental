B.hcr: B.hs
	ghc -c -fext-core -fforce-recomp B.hs

C.hcr: C.hs
	ghc -c -fext-core -fforce-recomp C.hs

run: B.hcr 
	../dist/build/incrm/incrm B.hcr

runC: C.hcr 
	../dist/build/incrm/incrm C.hcr

clean:
	rm *.hcr *.o *.hi

