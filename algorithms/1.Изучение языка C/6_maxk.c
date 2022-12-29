#include <stdio.h>
#include <iso646.h>

int main (int argc, char* argv[])
{
    int n, k, max;
    max=-100000000;
    scanf("%d", &n);
    int A[n];
    for(int i=0; i<n; ++i)
    {
        scanf("%d", &A[i]);
    }
    scanf("%d", &k);
    int sum;
    sum=0;
    for(int i=0; i<k; ++i)
    {  
        sum+=A[i];
    }
    max=sum;
    for(int i=0; i+k<n; ++i)
    {
        sum=sum-A[i]+A[k+i];
        max=(sum>max)? sum : max;
    }
    printf("%d", max);
    return 0;
}
