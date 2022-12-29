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

int ListLength(struct Elem* l)
{
    int len=0;
    struct Elem* x=l;
    while(x->next!=l)
    {
        len+=1;
        x=x->next;
    }
    return len;
}

struct Elem* ListSearch(struct Elem* l, long int v)
{
    struct Elem* x=l->next;
    while(x!=l and x->v!=v)
        x=x->next;
}

void swap(struct Elem* a, struct Elem* b)
{
    struct Elem* tmpan, *tmpbn, *tmpap, *tmpbp, *tmp;
    tmpan=a->next;
    tmpbn=b->next;
    tmpap=a->prev;
    tmpbp=b->prev;
    long int tmpv=a->v;
    a->v=b->v;
    b->v=tmpv;
}

void InsertSort(struct Elem* l)
{
    struct Elem* x=l;
    struct Elem* m=l;
    for(int i=1; i<ListLength(l); ++i)
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
    struct Elem* z=x->next;//l
    x->next=y;//l->next=val//l
    y->prev=x;//val->prev=l//val
    y->next=z;//val->next=l//l
    z->prev=y;//l->prev=val//
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
    InsertSort(l);
    while(l->v!=-9909)
        {printf("%ld ", l->v);
        l=l->next;}
    return 0;
}
