#include <stdio.h>

void functionA(int n);

void functionB(int n);

void functionC(int n);

void functionA(int n) {
    if (n > 0) {
        functionB(n - 1);
    }
}

void functionB(int n) {
    if (n > 0) {
        functionC(n - 1);
    }
}

void functionC(int n) {
    if (n > 0) {
        functionA(n - 1);
    }
}

int main() {
	int arr[3];
	arr[0] = 1;
	arr[2] = 14;
    functionA(3);
    return 0;
}
