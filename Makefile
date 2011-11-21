all: demo

demo: Main.hs Data/HEPEVT.hs
	ghc -o $@ Main.hs
