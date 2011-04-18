TARGETS = eval.o file_io.o parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o mickey
CXX = llvm-g++
CXXFLAGS = -Wall -Iinclude

all: $(TARGETS)

mickey: eval.o file_io.o parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o

check: all
	./mickey test.scm

clean:
	rm -f $(TARGETS)
