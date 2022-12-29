#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdlib.h>

struct queue
{
    long long int* data;
    int cap;
    int count;
    int head;
    int tail;
};

void InitQueue(struct queue* q)
{
    q->data=(long long int*)malloc(sizeof(long long int)*4);
    q->cap=4;
    q->count=q->head=q->tail=0;
}

bool QueueEmpty(struct queue* q)
{
    return q->count==0 ? true : false;
}

void Enqueue(struct queue* q, long long int a)
{
    q->data[q->tail]=a;
    q->tail+=1;
    q->count+=1;
    if(q->tail==q->cap)
    {
        q->data=(long long  int*)realloc(q->data, sizeof(long long int)*2*q->cap);
        q->cap*=2;
    }
    if(q->tail==q->cap)
        q->tail=0;
}

long long int Dequeue(struct queue* q)
{
    long long int x=q->data[q->head];
    q->head+=1;
    if(q->head==q->cap)
        q->head=0;
    q->count-=1;
    return x;
}

void ENQ(struct queue* q, long long int x)
{
    Enqueue(q, x);
}

void EMPTY(struct queue* q)
{
    if(QueueEmpty(q))
        printf("true\n");
    else
        printf("false\n");
}

void DEQ(struct queue* q)
{
    long long int x=Dequeue(q);
    printf("%lld\n", x);
}

int main(int argc, char* argv[])
{
    struct queue que;
    InitQueue(&que);
    long long int x;
    char X[20];
    char s[100];
    gets(s);
    while(s[0]!='E' or s[1]!='N' or s[2]!='D')
    {
        if(s[0]=='E' and s[1]=='N' and s[2]=='Q')
        {
            int i=4;
            while(s[i]!='\0')
            {
                X[i-4]=s[i];
                i+=1;
            }
            X[i-4]='\0';
            x=atol(X);
            ENQ(&que, x);
        }
        else if(s[1]=='M')
        {
            EMPTY(&que);
        }
        else if(s[0]=='D')
        {
            DEQ(&que);
        }
        gets(s);
    }
    free(que.data);
    return 0;
}
