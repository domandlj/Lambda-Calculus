name = mylamb

build:
	ghc Main.hs -o $(name)
clean:
	rm *.o
	rm *.hi
