CXX      = llvm-g++
CXXFLAGS = -Wall -Iinclude -DUSE_READLINE # -DBOEHM_GC
#CXXFLAGS = -Wall -Iinclude -DUSE_READLINE -O6 -ffast-math -fomit-frame-pointer
LDFLAGS  = -lreadline # -lgc

TARGETS_O = backtrace.o \
            heap.o \
            cons.o \
            apply.o \
            primitives.o \
            types.o \
            eval.o \
            file_io.o \
            parser.o \
            print.o \
            primops.o \
            tokenizer.o \
            tests.o \
            util.o \
            repl.o \
            test.o

TARGETS = $(TARGETS_O) mickey

PORTABLE_TESTS = tests/hello.scm \
                 tests/begin.scm \
                 tests/math.scm  \
                 tests/strings.scm

all: $(TARGETS)

mickey: $(TARGETS_O)

check: all
	echo "(run-tests)" | ./mickey
	./mickey tests/*

check-scheme: all
	cd tests ; ../mickey tests.scm

diff: all
	# mickey and chicken should have same output
	@echo "=== Chicken Scheme ==="
	@csi -bq $(PORTABLE_TESTS)
	@echo ""
	@echo "=== Mickey Scheme ==="
	@./mickey $(PORTABLE_TESTS)

tarball: clean
	rm -rf dist/mickey4
	mkdir -p dist/mickey4
	mkdir dist/mickey4/tests
	mkdir dist/mickey4/include
	cp -R AUTHOR BUGS COPYING INSTALL Makefile README TODO *.cpp dist/mickey4
	cp -R include/* dist/mickey4/include/
	cp -R tests/* dist/mickey4/tests/
	cd dist; tar cfz mickey4-`date +%Y-%m-%d`.tar.gz mickey4
	rm -rf dist/mickey4

backup: tarball
	cp dist/mickey4-`date +%Y-%m-%d`.tar.gz ~/Dropbox/koding/

clean:
	rm -f $(TARGETS)
