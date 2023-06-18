CC = clang++
cxxflags = $(shell llvm-config --cxxflags) -g
ldflags = $(shell llvm-config --ldflags)
ldflags += $(shell llvm-config --libs)
ldflags +=  -Wl,--export-dynamic

toy:toy.o
	$(CC) toy.o -o toy $(ldflags)

toy.o:toy.cpp KaleidoscopeJIT.h
	$(CC) -c toy.cpp $(cxxflags)

.PHONY:clean

.PHONY:test
test:
	./toy < fib.ks |& clang -x ir -
	# gdb a.out

clean:
	rm *.o toy