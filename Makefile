EXECUTABLE=behind

all: behind.hs
	ghc -o $(EXECUTABLE) -package hscurses -hide-package monads-tf behind.hs

clean:
	rm $(EXECUTABLE)
	rm *.o
	rm *.hi