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
            options.o \
            tokenizer.o \
            file_io.o \
            util.o \
            parse-string.o \
            parser.o \
            types.o \
            module.o \
            module_import.o \
            module_write.o \
            module_math.o \
            module_base.o \
            module_process-context.o \
            assertions.o \
            circular.o \
            print.o \
            arguments.o \
            repl.o \
            primops.o \
            evlis.o \
            eval.o \
            backtrace.o \
            syntax-rules.o \
            cons.o \
            tests.o

TARGETS = $(TARGETS_O) mickey

all: $(TARGETS)

mickey: $(TARGETS_O)

clean:
	rm -f $(TARGETS)
