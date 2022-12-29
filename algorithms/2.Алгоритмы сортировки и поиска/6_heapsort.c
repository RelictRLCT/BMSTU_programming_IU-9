#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <iso646.h>
#include <string.h>

void swap(void* a, void* b, size_t width)
{
    void* c=malloc(width);
    memcpy(c, a, width);
    memcpy(a, b, width);
    memcpy(b, c, width);
    free(c);
}

void Heapify(int i, size_t n, void* base,
             int (*compare)(const void *a, const void *b), size_t width)
{
    while(true)
    {
        int l=2*i+1;
        int r=l+1;
        int j=i;
        if(l<n and compare((char*)base+i*width, (char*)base+l*width)<0)
            i=l;
        if(r<n and compare((char*)base+i*width, (char*)base+r*width)<0)
            i=r;
        if(i==j)
            break;
        swap((char*)base+i*width, (char*)base+j*width, width);
    }
}

void BuildHeap(size_t n, void* base,
               int (*compare)(const void *a, const void *b), size_t width)
{
    int i=(n/2)-1;
    while(i>=0)
    {
        Heapify(i, n, base, compare, width);
        i-=1;
    }
}

void hsort(void *base, size_t nel, size_t width,
        int (*compare)(const void *a, const void *b))
{
    BuildHeap(nel, base, compare, width);
    int i=nel-1;
    while(i>0)
    {
        swap((char*)base, (char*)base+i*width, width);
        Heapify(0, i, base, compare, width);
        i-=1;
    }
}

int compar(void* aa, void* bb)
{
    char* a=*((char**)aa);
    char* b=*((char**)bb);
    int lena=strlen(a);
    int lenb=strlen(b);
    int counta=0;
    int countb=0;
    for(int i=0; i<lena; ++i)
        if(a[i]=='a')
            counta+=1;
    for(int i=0; i<lenb; ++i)
        if(b[i]=='a')
            countb+=1;
    if(counta>countb)
        return 18;
    if(counta<countb)
        return -12;
    return 0;
}

int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);
    char musor;
    scanf("%c", &musor);
    char** strs=(char**)malloc(sizeof(char*)*n);
    for(int i=0; i<n; ++i)
    {
        char* str=(char*)malloc(1);
        char c;
        scanf("%c", &c);
        int j=0;
        while(c!='\n')
        {
            str[j]=c;
            j+=1;
            str=(char*)realloc(str, j+1);
            scanf("%c", &c);
        }
        str[j]='\0';
        strs[i]=str;
    }

    hsort(strs, n, sizeof(char*), compar);

    for(int i=0; i<n; ++i)
        printf("%s ", strs[i]);

    for(int i=0; i<n; ++i)
        free(strs[i]);
    free(strs);
    return 0;
}
