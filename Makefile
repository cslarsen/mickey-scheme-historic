TARGETS = apply.o primitives.o types.o eval.o file_io.o parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o mickey
CXX = llvm-g++
CXXFLAGS = -g -Wall -Iinclude # -Weffc++
all: $(TARGETS)

mickey: apply.o primitives.o types.o eval.o file_io.o parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o

check: all
	echo ":TEST" | ./mickey
	./mickey tests/hello.scm \
	         tests/math.scm \
	         tests/begin.scm

clean:
	rm -f $(TARGETS)
