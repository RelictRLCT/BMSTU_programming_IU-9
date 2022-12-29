#include <stdio.h>
#include <iso646.h>
#include <stdbool.h>

void bubblesort(unsigned long nel,
        int (*compare)(unsigned long i, unsigned long j),
        void (*swap)(unsigned long i, unsigned long j))
{
    int c=1;
    int j=nel;
    int i=0;
    while(j>i)
        {
            for(int i1=i; i1<j-1; ++i1)
            {
                    if(compare(i1,i1+1)==1)
                        swap(i1,i1+1);
            }
            j-=1;
            for(int j1=j-1; j1>i; --j1)
            {
                if(compare(j1,j1-1)==-1)
                        swap(j1,j1-1);
            }
            i+=1;
        }
}
