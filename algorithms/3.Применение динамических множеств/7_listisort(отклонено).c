#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdlib.h>

struct Elem {
    struct Elem *prev, *next;
    long int v;
};

bool ListEmpty(struct Elem* l)
{
    return (l->next==l) ? true : false;
}

void swap(struct Elem* a, struct Elem* b)
{
    long int locv=a->v;
    a->v=b->v;
    b->v=locv;
}

void InsertSort(struct Elem* l, int len)
{
    struct Elem* x=l;
    struct Elem* m=l;
    for(int i=1; i<len; ++i)
    {
        int count=0;
        int countmin=0;
        x=l;
        m=l;
        long int min=2147483647;
        for(int j=0; j<i-1; ++j)
        {
            x=x->next;
        }
        m=x->next;
        while(m->next!=l)
        {
            count+=1;
            if(m->v < min)
            {
                countmin=count;
                min=m->v;
            }
            m=m->next;
        }
        m=x;
        for(int j=0; j<countmin; ++j)
            m=m->next;
        if (x->v > m->v) swap(x, m);
    }
}

void InsertAfter(struct Elem* x, struct Elem* y)
{
    struct Elem* z=x->next;
    x->next=y;
    y->prev=x;
    y->next=z;
    z->prev=y;
}

int main(int argc, char* argv[])
{
    struct Elem element;
    element.v=-9909;
    struct Elem* l;
    l=&element;
    l->next=l;
    l->prev=l;
    struct Elem* list=l;
    int n;
    long int valuev;//Николай Валуев
    scanf("%d", &n);
    struct Elem val[n];
    for(int i=0; i<n; ++i)
    {
        scanf("%ld", &valuev);
        val[i].v=valuev;
        InsertAfter(l, &val[i]);
        l=l->next;
    }
    l=l->next->next;
    InsertSort(l, n);
    while(l->v!=-9909)
    {
        printf("%ld ", l->v);
        l=l->next;
    }
    return 0;
}
