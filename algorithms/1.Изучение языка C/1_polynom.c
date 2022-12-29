#include <stdio.h>
#include <math.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
    long int n, x0;
    scanf("%ld %ld", &n, &x0);
    long int* A=(long int*)malloc((n+1)*sizeof(long int));
    for (long int i=0; i<n+1; ++i)
    {
        scanf("%ld", &A[i]);
    }

    long int res=0;

    for(long int i=0; i<n; ++i)
    {
        res=(res+A[i])*x0;
    }

    res=res+A[n];

    printf("%ld ", res);

    long int pro;
    pro=0;
    long int n1=n;
    long int pow[n];
    for (long int i=0; i<n; ++i)
    {
        pow[i]=1;
        long int n1=n-i;
        while(n1!=1)
            {
                 pow[i]*=x0;
                 n1-=1;
            }
    }
    for (long int i=0; i<n; ++i)
    {
        n1-=1;
        pro+=(n1+1)*A[i]*pow[i];
    }
    printf("%ld\n", pro);
    free(A);
    return 0;
}
