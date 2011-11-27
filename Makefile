all: demo

demo: Main.hs Data/HEPEVT.hs
	ghc -o $@ Main.hs

clean:
	rm -f demo *.o *.hi
