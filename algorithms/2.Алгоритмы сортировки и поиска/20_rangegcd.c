#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

long int gcd (long int a, long int b)
{
    return llabs(b) ? gcd(llabs(b), llabs(a%b)) : llabs(a);
}


void compute_log(int m, int* lg)
{
    int i=1;
    int j=0;
    while(i<=m)
    {
        while(j<(1<<i))
        {
            lg[j]=i-1;
            j+=1;
        }
        i+=1;
    }
}

void sparse_build(long int* A, int* lg, long int** ST, int n)
{
    int m=lg[n]+1;
    int i=0;
    while(i<n)
    {
        ST[i][0]=A[i];
        i+=1;
    }
    int j=1;
    while(j<m)
    {
        i=0;
        while(i<=n-(1<<j))
        {
            ST[i][j]=gcd(ST[i][j-1], ST[i+(1<<(j-1))][j-1]);
            i+=1;
        }
        j+=1;
    }
}

long int sparse_query(long int** ST, int l, int r, int* lg)
{
    int j=lg[r-l+1];
    return gcd(ST[l][j], ST[r-(1<<j)+1][j]);
}

int main(int argc, char* argv[])
{
    int n=0;
    scanf("%d", &n);
    long int A[n];
    for(int i=0; i<n; ++i)
        scanf("%ld", &A[i]);
    int m=0;
    scanf("%d", &m);
    int* lg=(int*)malloc(sizeof(int)*(1<<(int)(log2(n)+1)));
    compute_log((log2(n)+1), lg);//i от 1!

    int mm=lg[n]+1;
    long int** ST=(long int**)malloc(sizeof(long int*)*n);
    for(int i=0; i<n; ++i)
        ST[i]=(long int*)malloc(sizeof(long int)*mm);

    sparse_build(A, lg, ST, n);

    int B[2]={0, 0};
    int pr[m];
    for(int i=0; i<m; ++i)
    {
        scanf("%d", &B[0]);
        scanf("%d", &B[1]);
        printf("%ld\n", sparse_query(ST, B[0], B[1], lg));
    }
    for(int i=0; i<n; ++i)
        free(ST[i]);
    free(ST);
    free(lg);
    return 0;
}
