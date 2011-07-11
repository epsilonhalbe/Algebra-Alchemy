all :
	ghc --make -Wall Algebra-Alchemy.hs

clean :
	rm -f Algebra-Alchemy tAlgebra-Alchemy tColor *.hi *.o report.html

test :
	ghc --make -Wall tAlgebra-Alchemy.hs

	./tAlgebra-Alchemy

prove :
# cpan App::Prove
	prove --exec make test


provehtml :
# cpan App::Prove::Plugin::HTML
	prove -P HTML=outfile:report.html --exec make test
