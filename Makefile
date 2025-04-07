all:
	cobc -c encrypt.cbl
	cobc -c decrypt.cbl
	cobc -c -x main.cbl
	cobc -x -o main encrypt.o decrypt.o main.o

clean:
	rm *.o 
	rm main