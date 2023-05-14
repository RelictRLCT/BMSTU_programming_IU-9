#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <math.h>
#include <stdlib.h>

void insert(int low, int high, int *A)
{
    for(int i = low; i <= high; i++)
    {
	int elem=A[i];
        int k=i-1;
	while(k >= low and abs(A[k]) > abs(elem))
	{
	    A[k+1]=A[k];
	    k-=1;
	}
	A[k+1]=elem;
    }
}

void merge(int k, int l, int m, int *A)
{
    int T[m - k + 1];
    int i = k;
    int j = l + 1;
    int h = 0;

    while(h < m - k + 1)
    {
	if(j <= m and (i == l + 1 or abs(A[j]) < abs(A[i])))
	{
       	    T[h] = A[j];
	    j+=1;
	}
	else
	{
            T[h] = A[i];
            i+=1;
	}
	h+=1;
    }
    int h1=0;
    for(int i = k; i <= m; ++i)
    {
        A[i] = T[h1];
        h1+=1;
    }
}

void mergesort_rec(int low, int high, int *A, int n);

void mergesort(int *A, int n)
{
    mergesort_rec(0, n - 1, A, n);
}

void mergesort_rec(int low, int high, int *A, int n)
{
    if(high - low<5)
    {
        insert(low, high, A);
    }
    else
        if (low<high)
        {
            int med = (low + high) / 2;
            mergesort_rec(low, med, A, n);
            mergesort_rec(med + 1, high, A, n);
            merge(low, med, high, A);
        }
}


int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);
    int A[n];
    for(int i = 0; i < n; ++i)
        scanf("%d", &A[i]);

    mergesort(A, n);

    for(int i = 0; i < n; i++)
        printf("%d ", A[i]);

    return 0;
}
