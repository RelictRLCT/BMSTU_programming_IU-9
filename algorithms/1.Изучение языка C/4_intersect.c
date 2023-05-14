#include <stdint.h>
#include <stdio.h>


int checkbit(int value, int position) {
    int result;
    if ((value & (1 << position)) == 0) {
        result = 0;
    } else {
        result = 1;
    }
    return result;
}

int main(int argc, char* argv[])
{
    int32_t a, b, res;
    int n1, n2;
    a=b=0;
    int a1, b1;
    scanf("%d", &n1);
    for(int i=0; i<n1; ++i)
    {
        scanf("%d", &a1);
        a=(a | (1 << a1));
    }
    scanf("%d", &n2);
    for(int i=0; i<n2; ++i)
    {
        scanf("%d", &b1);
        b=(b | (1 << b1));
    }
    res= a & b;

    for(int i=0; i<32; ++i)
    {
        if (checkbit(res, i) == 1)
            printf("%d ", i);
    }

}
