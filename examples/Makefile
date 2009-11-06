all := Fibber Tiny

all: $(all)

%: %.hs
	ghc -O --make $<

clean:
	-rm -f *.hi *.o $(all)
