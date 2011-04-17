TARGETS = test.o mickey

all: $(TARGETS)

mickey: test.o

check: all
	./mickey test.scm

clean:
	rm -f $(TARGETS)
