#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdlib.h>

struct Task {
    int low, high;
};

struct stack
    {
        struct Task* data;
        int cap;
        int top;
    };

void InitStack(struct stack* s, int n)
{
    s->data=(struct Task *)malloc(sizeof(struct Task)*n);
    s->cap=n;
    s->top=0;
}

bool StackEmpty(struct stack* s)
{
    return (s->top==0 ? true : false);
}

void Push(struct stack* s, struct Task x)
{
    s->data[s->top]=x;
    s->top+=1;
}

void Pop(struct stack* s, struct Task* x)
{
    s->top-=1;
    x->low=s->data[s->top].low;
    x->high=s->data[s->top].high;
}

int Partition(int low, int high, int* A)
{
    int i=low;
    int j=low;

    while(j<high)
    {
        if (A[j]<A[high])
           {
                int tmp=A[i];
                A[i]=A[j];
                A[j]=tmp;
                i+=1;
           }
        j+=1;

    }
    int tmp = A[i];
    A[i]=A[high];
    A[high]=tmp;
    return i;
}

void QuickSortNotRec(int n, int* A)
{
    struct stack tasks;
    struct Task newtasks;
    InitStack(&tasks, n);
    newtasks.low=0;
    newtasks.high=n-1;
    Push(&tasks, newtasks);
    while(not StackEmpty(&tasks))
    {
        struct Task onetask;
        Pop(&tasks, &onetask);
        if(onetask.low<onetask.high)
        {
            struct Task newonetask;
            int i=Partition(onetask.low, onetask.high, A);

            newonetask.low=i+1;
            newonetask.high=onetask.high;
            Push(&tasks, newonetask);

            newonetask.low=onetask.low;
            newonetask.high=i-1;
            Push(&tasks, newonetask);
        }
    }
    free(tasks.data);
}

int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);
    int A[n];
    for(int i=0; i<n; ++i)
        scanf("%d", &A[i]);
    QuickSortNotRec(n, A);
    for(int i=0; i<n; ++i)
        printf("%d ", A[i]);
    return 0;
}
