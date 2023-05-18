build:
	cabal build && \
	cp dist-newstyle/build/x86_64-linux/ghc-8.8.4/latte-1.0.0.0/build/latte/latte ./interpreter

generate_grammar:
	BNFC=/home/students/inf/PUBLIC/MRJP/bin/bnfc && \
	mkdir -p generated && \
	cd generated && \
	$(BNFC) -m ../latte.cf && \
	make

.PHONY: clean
clean:
	rm -rf generated
	rm interpreter
