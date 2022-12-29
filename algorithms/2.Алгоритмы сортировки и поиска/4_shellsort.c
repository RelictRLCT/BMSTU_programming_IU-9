#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>

void shellsort(unsigned long nel,
        int (*compare)(unsigned long i, unsigned long j),
        void (*swap)(unsigned long i, unsigned long j))
{
    int FIB1[nel];
    FIB1[0]=FIB1[1]=1;
    int i=2;
    while(true)
    {
        FIB1[i]=FIB1[i-1]+FIB1[i-2];
        if (FIB1[i]>=nel) break;
        i+=1;
    }
    int FIB[i-1];
    for(; i<nel; ++i)
    {
        FIB1[i]=-1;
    }
    int j=0;
    for(i=i-1; i>0; --i)
    {
        if (FIB1[i]!=-1)
        {FIB[j]=FIB1[i];
        j+=1;}
    }
    int fibm=j;
    int k;
    for(int i=0; i<fibm; ++i)
        for(int j=FIB[i]; j<nel; j+=FIB[i])
        {
            k=j-FIB[i];
            while(k>=0)
            {if ((compare (j, k))==1 )
                {swap(k, j);
                }k-=FIB[i];}
        }
}
