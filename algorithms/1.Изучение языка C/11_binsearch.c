#include <iso646.h>
#include <stdio.h>

unsigned long binsearch(unsigned long nel, int (*compare)(unsigned long i))
{
    unsigned long i=0;
    unsigned long left=0;
    unsigned long right=nel-1;
    unsigned long nel1=nel;
    unsigned long ss=-1;
    while (left<=right)
    {
            i=(unsigned long)((right+left)/2);
            if(compare(i)==0)
                return i;
            else if (compare(i)>0)
            {
                right=i-1;
            }
            else
            {
                left=i+1;
            }
    }
    if (ss==-1)
        return nel1;
}
