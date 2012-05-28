Inctime.hcr:
	touch Inctime.hcr

B.hcr: B.hs
	ghc -c -fext-core B.hs

run: Inctime.hcr B.hcr
	../dist/build/incrm/incrm

