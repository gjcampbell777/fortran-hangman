main: hangman.f95
	gfortran -o hangman hangman.f95

77: hang4.for
	gfortran -o hangman hang4.for

clean:
	rm hangman