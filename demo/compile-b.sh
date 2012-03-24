(cd ..; cabal build) && \
echo built && \
../dist/build/Incrementaliser/Incrementaliser B && cat B.hcr && as B.S -o B.o
