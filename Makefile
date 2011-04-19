TARGETS = primitives.o types.o eval.o file_io.o parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o mickey
CXX = g++
CXXFLAGS = -g -Wall -Iinclude

all: $(TARGETS)

mickey: primitives.o types.o eval.o file_io.o parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o

check: all
	echo ":TEST" | ./mickey
	./mickey test.scm

clean:
	rm -f $(TARGETS)
