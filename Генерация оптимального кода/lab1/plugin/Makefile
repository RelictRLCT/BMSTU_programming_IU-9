phi-debug:
	mkdir -p plugin
	g++ -g -Wall -Wextra -std=c++14 \
	    -I`gcc -print-file-name=plugin`/include \
	    -fPIC -fno-rtti -shared \
	    -DPREFERRED_DEBUGGING_TYPE=DWARF2_DEBUG \
	    -DHAVE_DEBUG_DWARF2 \
	    src/phi-debug/phi-debug.cpp \
	    -o plugin/phi-debug.so

test:
	gcc -fplugin=plugin/phi-debug.so -O3 src/test/test.c -o src/test/test
	rm src/test/test

.PHONY: clean
clean:
	rm -rf plugin src/test/test
