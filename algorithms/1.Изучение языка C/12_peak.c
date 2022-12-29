#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>

unsigned long peak(unsigned long nel,
        int (*less)(unsigned long i, unsigned long j))
{
    unsigned long i=0;
    for(i=1; i<nel-1; ++i)
    {
        if ((less(i-1, i)==1)and(less(i, i+1)==0))
            return i;
    }
}
