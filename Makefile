CXX      = llvm-g++
CXXFLAGS = -g -Wall -Iinclude -DUSE_READLINE # -DBOEHM_GC
#CXXFLAGS = -Wall -Iinclude -DUSE_READLINE -O6 -ffast-math -fomit-frame-pointer
LDFLAGS  = -lreadline # -lgc

TARGETS_O = assertions.o \
            module.o \
            module_base.o \
            module_math.o \
            options.o \
            backtrace.o \
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
	./mickey -Itests tests/*.scm

check-scheme: all
	./mickey -Itests tests/tests.scm

diff: all
	# mickey and chicken should have same output
	@echo "=== Chicken Scheme ==="
	@csi -bq $(PORTABLE_TESTS)
	@echo ""
	@echo "=== Mickey Scheme ==="
	@./mickey $(PORTABLE_TESTS)

tarball: clean
	make clean
	rm -f dist/*
	cd .. ; tar cfz mickey4-`date +%Y-%m-%d`.tar.gz mickey4 ; mv mickey4-`date +%Y-%m-%d`.tar.gz mickey4/dist

backup: tarball
	cp dist/mickey4-`date +%Y-%m-%d`.tar.gz ~/Dropbox/koding/

clean:
	rm -f $(TARGETS)
