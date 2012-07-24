LLVM_CONFIG   = llvm-config
LLVM_CXXFLAGS = ${shell $(LLVM_CONFIG) --cxxflags}
LLVM_LDFLAGS  = ${shell $(LLVM_CONFIG) --ldflags --libs}

CXX      = llvm-g++
CXXFLAGS = -g -W -Wall -I../include -DUSE_LLVM -DUSE_READLINE -DNO_EXCEPTIONS ${LLVM_CXXFLAGS}
LDFLAGS  = -lreadline ${LLVM_LDFLAGS}

TARGETS_O = exceptions.o \
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
            evlis.o \
            eval.o \
            backtrace.o \
            module_base.o \
            syntax-rules.o \
            cons.o \
            tests.o

TARGETS = $(TARGETS_O) mickey

all: $(TARGETS)

mickey: $(TARGETS_O)

clean:
	rm -f $(TARGETS)
