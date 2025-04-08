all:
	cobc -c src/encrypt.cbl
	cobc -c src/decrypt.cbl
	cobc -c -x src/main.cbl
	cobc -x -o main encrypt.o decrypt.o main.o

clean:
	rm *.o 
	rm main