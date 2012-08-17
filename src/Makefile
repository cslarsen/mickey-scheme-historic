LLVM_CONFIG   = llvm-config
LLVM_CXXFLAGS = ${shell $(LLVM_CONFIG) --cxxflags}
LLVM_LDFLAGS  = ${shell $(LLVM_CONFIG) --ldflags --libs}

CXX      = llvm-g++
CXXFLAGS = -g \
					 -W \
					 -Wall \
					 -I../include \
					 -DUSE_LLVM \
					 -DUSE_READLINE \
					 -DNO_EXCEPTIONS \
					 ${LLVM_CXXFLAGS}

LDFLAGS  = -lreadline ${LLVM_LDFLAGS}
LIB_FLAGS = -shared -fPIC $(LDFLAGS)

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
            module_load.o \
            module_char.o \
            module_write.o \
            module_math.o \
            module_mickey_environment.o \
            module_base.o \
            module_process-context.o \
            module_mickey_environment.o \
            module_mickey_dynamic_library.o \
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

TARGETS_SO = libmickey-uname.so

LIB_DEPS =  exceptions.o \
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
            module_load.o \
            module_char.o \
            module_write.o \
            module_math.o \
            module_mickey_environment.o \
            module_base.o \
            module_process-context.o \
            module_mickey_dynamic_library.o \
            assertions.o \
            circular.o \
            print.o \
            arguments.o \
            primops.o \
            evlis.o \
            eval.o \
            backtrace.o \
            syntax-rules.o \
            cons.o

TARGETS = $(TARGETS_O) $(TARGETS_SO) mickey

lib%.so: %.cpp $(LIB_DEPS)
	$(CXX) $(LIB_FLAGS) $(CXXFLAGS) $< -o $@ $(LIB_DEPS)

all: $(TARGETS)

mickey: $(TARGETS_O)

clean:
	rm -f $(TARGETS)
