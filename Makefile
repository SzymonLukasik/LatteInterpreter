run:
	cabal run

generate_grammar:
	# mkdir -p generated && \
	cd generated && \
	bnfc -m ../Lakke.cf && \
	make

.PHONY: clean
clean:
	rm -rf generated
