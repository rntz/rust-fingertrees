fingerdeque: fingerdeque.rs fingertree.rs FORCE
	rustc $<

.PHONY: clean FORCE
clean:
	rm -f fingertree
