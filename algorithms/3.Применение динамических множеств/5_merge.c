#include <stdio.h>
#include <iso646.h>
#include <stdbool.h>
#include <stdlib.h>

struct tri
{
    int index;
    int k;
    int v;
};

struct queue
{
    struct tri* heap;
    int cap;
    int count;
};

void initpqueue(struct queue* q, int n)
{
    q->heap=(struct tri*)malloc(sizeof(struct tri)*n);
    q->cap=n;
    q->count=0;
}

int maximum(struct queue* q)
{
    return q->heap[0].v;
}

bool queueempty(struct queue* q)
{
    return q->count == 0? true : false;
}

void insert(struct queue* q, struct tri p)
{
    int i=q->count;
    q->count=i+1;
    q->heap[i]=p;
    while(i>0 and q->heap[(i-1)/2].k < q->heap[i].k)
    {
        struct tri tmp=q->heap[(i-1)/2];
        q->heap[(i-1)/2]=q->heap[i];
        q->heap[i]=tmp;
        q->heap[i].index=i;
        i=(i-1)/2;
    }
    q->heap[i].index=i;
}

void heapify(int i, int n, struct tri* A)
{
    while(true)
    {
        int l=2*i+1;
        int r=l+1;
        int j=i;
        if ((l<n) and (A[i].k<A[l].k))
        {
            i=l;
        }
        if((r<n) and (A[i].k<A[r].k))
            i=r;
        if (i==j)
            break;
        int locv=A[i].v;
        A[i].v=A[j].v;
        A[j].v=locv;
        A[i].index=i;
        A[j].index=j;
        int lock=A[i].k;
        A[i].k=A[j].k;
        A[j].k=lock;
    }
}

int Extractmax(struct queue* q)
{
    struct tri p=q->heap[0];
    q->count-=1;
    if (q->count>0)
    {
        q->heap[0]=q->heap[q->count];
        q->heap[0].index=0;
        heapify(0, q->count, q->heap);
    }
    return p.v;
}

int main(int argc, char* argv[])
{
    int n;
    int k;
    int len=0;
    scanf("%d", &k);
    for(int i=0; i<k; ++i)
    {
        scanf("%d", &n);
        len+=n;
    }
    struct queue que;
    initpqueue(&que, len);
    for(int i=0; i<len; ++i)
    {
        scanf("%d", &n);
        struct tri troyka;
        troyka.index=i;
        troyka.v=n;
        troyka.k=-n;

        insert(&que, troyka);
    }

    for(int i=0; i<len; ++i)
        printf("%d ", Extractmax(&que));
    free(que.heap);
    return 0;
}
