CC = clang++
cxxflags = $(shell llvm-config --cxxflags) -g
ldflags = $(shell llvm-config --ldflags)
ldflags += $(shell llvm-config --libs)
ldflags +=  -Wl,--export-dynamic

toy:toy.o
	$(CC) toy.o -o toy $(ldflags)

toy.o:toy.cpp
	$(CC) -c toy.cpp $(cxxflags)

.PHONY:clean

.PHONY:test
test:
	./toy < test.txt
	./toy < test2.txt
	clang test_target_file.c output.o -o test_target_file
	./test_target_file
	clang -c lib.c
	./toy < test3.txt
	clang -o main output.o lib.o -no-pie
	./main

clean:
	rm *.o toy test_target_file main