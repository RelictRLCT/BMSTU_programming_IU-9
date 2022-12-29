#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdlib.h>

struct dstack
{
    long int* data;
    int cap;
    int top1;
    int top2;
};

void InitDoubleStack(struct dstack* s, int n)
{
    s->data=(long int*)malloc(sizeof(long int)*n);
    s->cap=n;
    s->top1=0;
    s->top2=n-1;
}

bool StackEmpty1(struct dstack* s)
{
    return s->top1==0 ? true : false;
}

bool StackEmpty2(struct dstack* s)
{
    return s->top2==s->cap-1 ? true : false;
}

void Push1(struct dstack* s, long int x)
{
    s->data[s->top1]=x;
    s->top1+=1;
}

void Push2(struct dstack* s, long int x)
{
    s->data[s->top2]=x;
    s->top2-=1;
}

long int Pop1(struct dstack* s)
{
    s->top1-=1;
    return s->data[s->top1];
}

long int Pop2(struct dstack* s)
{
    s->top2+=1;
    return s->data[s->top2];
}

void InitQueue(struct dstack* s, int n)
{
    InitDoubleStack(s, n);
}

bool QueueEmpty(struct dstack* s)
{
    return (StackEmpty1(s) and StackEmpty2(s)) ? true : false;
}

void Enqueue(struct dstack* s, long int x, struct dstack* maxs)
{
    Push1(s, x);
    if(maxs->top1==0)
        Push1(maxs, x);
    else
    {
        if(s->data[s->top1-1]>maxs->data[maxs->top1-1])
            Push1(maxs, x);
        else
            Push1(maxs, maxs->data[maxs->top1-1]);
    }
}

void Dequeue(struct dstack* s, struct dstack* maxs, int n)
{
    if(StackEmpty2(s))
        while(not StackEmpty1(s))
        {
            long int poped=Pop1(s);
            Push2(s, poped);
            if(maxs->top2==n-1)
                Push2(maxs, poped);
            else if(poped>maxs->data[maxs->top2+1])
                Push2(maxs, poped);
            else
                Push2(maxs, maxs->data[maxs->top2+1]);
            while(not StackEmpty1(maxs))
                Pop1(maxs);
        }
    Pop2(maxs);
    printf("%ld\n", Pop2(s));
}

void EMPTY(struct dstack* s)
{
    if(QueueEmpty(s)==true)
        printf("true\n");
    else
        printf("false\n");
}

int main(int argc, char* argv[])
{
    struct dstack que;
    struct dstack maxs;
    long int x;
    int n=100000;
    InitQueue(&que, n);
    InitDoubleStack(&maxs, n);
    char s[200];
    while(true)
    {
        scanf("%s", s);
        if(s[0]=='E')
        {
            if(s[1]=='N')
            {
                if(s[2]=='Q')
                {
                    scanf("%ld", &x);
                    Enqueue(&que, x, &maxs);
                }
                else break;
            }
            else
            {
                EMPTY(&que);
            }
        }
        else if(s[0]=='D')
        {
            Dequeue(&que, &maxs, n);
        }
        else
        {
            if(maxs.top1==0)
                printf("%ld\n", maxs.data[maxs.top2+1]);
            else if(maxs.top2==n-1)
                printf("%ld\n", maxs.data[maxs.top1-1]);
            else
                printf("%ld\n", maxs.data[maxs.top1-1]>maxs.data[maxs.top2+1]?
                       maxs.data[maxs.top1-1] : maxs.data[maxs.top2+1]);
        }
    }
    free(que.data);
    free(maxs.data);
    return 0;
}
