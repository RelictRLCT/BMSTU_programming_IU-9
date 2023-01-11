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

void swap(struct Elem* x, struct Elem* m, struct Elem* l)
{
    struct Elem* ins=m;//перевязка элементов, где стоял m
    m->next->prev=m->prev;
    m->prev->next=m->next;

    x->prev->next=ins;//вставка
    ins->prev=x->prev;
    x->prev=ins;
    ins->next=x;
}

void InsertSort(struct Elem* l, int len)
{
    struct Elem* x=l;
    struct Elem* m=l;
    bool swaped=false;
    for(int i=1; i<len+1; ++i)
    {
        swaped=false;
        x=l;
        for(int j=0; j<i-1; ++j)
        {
            x=x->next;
        }
        m=x;
        while(x->prev->v!=-9909 and x->prev->v > m->v)
        {
            x=x->prev;
            swaped=true;
        }
        if(swaped)
        {
            swap(x, m, l);
        }
        while(l->prev->v!=-9909)
            l=l->next;
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
    while(l->prev->v!=-9909)
        l=l->next;
    for(int i=0; i<n; ++i)
    {
        printf("%ld ", l->v);
        l=l->next;
    }
    return 0;
}
