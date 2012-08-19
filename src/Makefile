LLVM_CONFIG   = llvm-config
LLVM_CXXFLAGS = ${shell $(LLVM_CONFIG) --cxxflags}
LLVM_LDFLAGS  = ${shell $(LLVM_CONFIG) --ldflags --libs}

CXX      = llvm-g++
CXXFLAGS = -g -W -Wall \
					 -I../include \
					 -DUSE_READLINE \
					 -fno-exceptions \
					 -fno-rtti

LDFLAGS  =
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
            module_write.o \
            module_math.o \
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

TARGETS_SO = libmickey-uname.so \
						 libmickey-misc.so \
						 libscheme-char.so

LIB_DEPS =  apply.o \
            arguments.o \
            assertions.o \
            backtrace.o \
            circular.o \
            cons.o \
            eval.o \
            evlis.o \
            exceptions.o \
            file_io.o \
            module.o \
            module_base.o \
            module_import.o \
            module_load.o \
            module_math.o \
            module_mickey_dynamic_library.o \
            module_mickey_environment.o \
            module_process-context.o \
            module_write.o \
            options.o \
            parse-string.o \
            parser.o \
            primops.o \
            print.o \
            syntax-rules.o \
            tokenizer.o \
            types.o \
            util.o

TARGETS = $(TARGETS_O) $(TARGETS_SO) libmickey.so mickey

all: $(TARGETS)

lib%.so: %.cpp $(LIB_DEPS)
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) $(LIB_DEPS) $< -o $@

libmickey.so: $(LIB_DEPS)
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) $^ -o $@

libmickey-uname.so: libraries/libmickey-uname.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey $< -o $@

libmickey-misc.so: libraries/libmickey-misc.cpp libmickey.so
	$(CXX) -DUSE_LLVM $(CXXFLAGS) $(LLVM_CXXFLAGS) $(LLVM_LDFLAGS) \
		$(LIB_FLAGS) $(LIB_DEPS) -L. -lmickey $< -o $@

libscheme-char.so: libraries/libscheme-char.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey  $< -o $@

mickey: mickey.cpp libmickey.so tests.o test.o repl.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) tests.o test.o repl.o -L. -lmickey -lreadline $< -o $@

clean:
	rm -rf $(TARGETS) *dSYM
