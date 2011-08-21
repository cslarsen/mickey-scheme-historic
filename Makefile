LLVM_CXXFLAGS = ${shell /usr/local/Cellar/llvm/2.9/bin/llvm-config --cxxflags}
CXX      = llvm-g++
CXXFLAGS = -g -Wall -Iinclude -DUSE_READLINE -DNO_EXCEPTIONS ${LLVM_CXXFLAGS}
#CXXFLAGS = -Wall -Iinclude -DUSE_READLINE -O6 -ffast-math -fomit-frame-pointer
LDFLAGS  = -lreadline # -lgc

TARGETS_O = raise.o \
            heap.o \
            test.o \
            apply.o \
            module.o \
            options.o \
            tokenizer.o \
            file_io.o \
            util.o \
            parser.o \
            types.o \
            module_math.o \
            assertions.o \
            print.o \
            repl.o \
            primops.o \
            eval.o \
            backtrace.o \
            module_base.o \
            cons.o \
            tests.o

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
