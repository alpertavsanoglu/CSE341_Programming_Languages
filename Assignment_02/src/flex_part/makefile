all: clean compile run

compile: 
	@echo "-------------------------------------------"
	@echo "Compiling..."
	flex -o gpp_lexer.c gpp_lexer.l
	gcc -o test gpp_lexer.c -lm 

run:
	@echo "-------------------------------------------"
	@echo "Running the tests...."
	@echo "================================================================================="
	./test test_input.gpp
	@echo "================================================================================="
	@echo "Completed tests...."

clean:
	@echo "-------------------------------------------"
	@echo "Removing compiled files..."
	@rm -f *.o
	@rm -f test
