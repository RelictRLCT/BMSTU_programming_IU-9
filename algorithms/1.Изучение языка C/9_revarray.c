#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <iso646.h>
#include <string.h>

void revarray(void *base, size_t nel, size_t width)
{
    void* tmp=(void*)malloc(width);
    for(int i=0; i<nel; ++i)
    {
        char* nac=(char*)base+i*width;
        char* kon=(char*)base+(nel-i-1)*width;
        memcpy(tmp, (void*)kon, width);
        memcpy((void*)kon, (void*)nac, width);
        memcpy((void*)nac, tmp, width);
        if(i==nel/2-1)
            break;
    }
    free(tmp);
}
