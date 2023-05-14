#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdlib.h>

void shellsort(unsigned long nel,
        int (*compare)(unsigned long i, unsigned long j),
        void (*swap)(unsigned long i, unsigned long j))
{
    int* fib=(int*)malloc(sizeof(int)*2);
    fib[0]=fib[1]=1;
    int i=1;
    while(fib[i]<nel)
    {
        i+=1;
        fib=(int*)realloc(fib, sizeof(int)*(i+1));
        fib[i]=fib[i-2]+fib[i-1];
    }

    int F[i-1];
    for(int j=0; j<i-1; ++j)
        F[j]=fib[i-1-j];

    for(int j=0; j<i-1; ++j)
    {
        int ii=F[j];
            while(ii<nel)
            {
                int k=ii;
                while(k-F[j]>=0 and compare(k-F[j], k)==1)
                {
                    swap(k-F[j], k);
                    k-=F[j];
                }
                ii+=F[j];
            }
    }
    free(fib);
}
