#include <stdio.h>
#include <stdlib.h>
#include <iso646.h>
#include <string.h>

int maxarray(void *base, size_t nel, size_t width,
            int (*compare)(void *a, void *b))
{
    char* max=(char*)base;
    int maxind=0;
    for(size_t i=0; i<nel; ++i)
    {
       char* tmp1=(char*)base+(i*width);
       if ((compare((void*)tmp1, (void*)max))>0)
       {
           max=(char*)tmp1;
           maxind=i;
       }
    }
    return maxind;
}
