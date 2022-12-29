#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>

int Partition(int low, int high, int* A)
{
    int i=low;
    int j=low;

    while(j<high)
    {
        if (A[j]<A[high])
           {
                int tmp=A[i];
                A[i]=A[j];
                A[j]=tmp;
                i+=1;
           }
        j+=1;

    }
    int tmp = A[i];
    A[i]=A[high];
    A[high]=tmp;
    return i;
}

void QuickSort(int n, int m, int* A)
{
    QuickSortRec(0, n-1, m, A);
}

void QuickSortRec(int low, int high, int m, int* A)
{
    if((low<high) and (high-low<m))
    {
        int q=Partition(low, high, A);
        QuickSortRec(low, q-1, m, A);
        QuickSortRec(q+1, high, m, A);
    }
    else SelectSort(low, high, A);
}

void SelectSort(int low, int high, int* A)
{
    int j=high;
    while(j>low)
    {
        int k=j;
        int i=j-1;
        while(i>=low)
        {
            if(A[k]<A[i])
                k=i;
            i-=1;
        }
        int tmp=A[j];
        A[j]=A[k];
        A[k]=tmp;
        j-=1;
    }
}

int main(int argc, char* argv[])
{
    int n,m;
    scanf("%d %d", &n, &m);
    int A[n];
    for(int i=0; i<n; ++i)
        scanf("%d", &A[i]);
    QuickSort(n, m, A);
    for(int i=0; i<n; ++i)
        printf("%3d", A[i]);
    return 0;
}
