TARGETS = cons.o apply.o primitives.o types.o eval.o file_io.o parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o mickey
CXX = llvm-g++
LDFLAGS = -lreadline
CXXFLAGS = -g -Wall -Iinclude # -Weffc++
all: $(TARGETS)

mickey: cons.o apply.o primitives.o types.o eval.o file_io.o parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o

check: all
	echo "(run-tests)" | ./mickey
	./mickey tests/hello.scm \
	 tests/math.scm \
	 tests/begin.scm \
	 tests/strings.scm

diff: all
	# mickey and chicken should have same output
	csi -bq tests/*; echo "========="; ./mickey tests/*

clean:
	rm -f $(TARGETS)
