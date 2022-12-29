#include <stdio.h>
#include <iso646.h>
#include <stdbool.h>
#include <stdlib.h>

unsigned long long int fibar(unsigned long long int *fib, unsigned long long int x)
{
    if (x>0) fib[0]=1;
    if (x>1) fib[1]=1;
    unsigned long long int i=2;
    while(true)
    {
        fib[i]=fib[i-1]+fib[i-2];
        if(fib[i] >= x) break;
        else i+=1;
    }
    return i;
}

int main(int argc, char* argv[])
{
    unsigned long long int x, index, j;
    scanf("%lld", &x);
    unsigned long long int x1=x;
    unsigned long long int* fib=(unsigned long long int*)malloc(500*sizeof(unsigned long long int));
    unsigned long long int* fibsys=(unsigned long long int*)malloc(500*sizeof(unsigned long long int));

   for(unsigned long long int i=0; i<500; ++i)
    {
        fib[i]=0;
        fibsys[i]=0;
    }
    index=fibar(fib, x);
    unsigned long long int i=0;
    j=0;
    for(; index>0; --index)
    {
        if(j>0)
            if(fibsys[j-1]==1)
            {
                fibsys[j]=0;
                j+=1;
                continue;
            }

        if (fib[index]<=x)
        {
            fibsys[j]=1;
            x-=fib[index];
            j+=1;
        }
        else {j+=1; continue;}

    }
    if (x1==0)
        printf("%d", 0);
    else if (x1==1)
        printf("%d", 1);
    else
    for(unsigned long long int i=0; i<j; ++i)
    {
        if((i==0) and (fibsys[i]==0))
            continue;
        printf("%lld", fibsys[i]);
    }
    free(fib);
    free(fibsys);
    return 0;
}
