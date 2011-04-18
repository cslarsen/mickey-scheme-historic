CXX = llvm-g++
CXXFLAGS = -Wall
TARGETS = parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o mickey

all: $(TARGETS)

mickey: parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o

check: all
	./mickey test.scm

clean:
	rm -f $(TARGETS)
