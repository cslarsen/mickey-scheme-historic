TARGETS = heap.o cons.o apply.o primitives.o types.o eval.o file_io.o parser.o print.o primops.o tokenizer.o tests.o util.o repl.o test.o mickey
CXX = llvm-g++
LDFLAGS = -lreadline -lgc
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

tarball: clean
	rm -rf dist/mickey4
	mkdir -p dist/mickey4
	cp -R AUTHOR BUGS COPYING INSTALL Makefile README TODO include/ tests/ *.cpp dist/mickey4
	cd dist; tar cfz mickey4-`date +%Y-%m-%d`.tar.gz mickey4
	rm -rf dist/mickey4

backup: tarball
	cp dist/mickey4-`date +%Y-%m-%d`.tar.gz ~/Dropbox/koding/

clean:
	rm -f $(TARGETS)
