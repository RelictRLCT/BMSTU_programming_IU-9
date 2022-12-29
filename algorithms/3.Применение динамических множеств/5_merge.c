#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdlib.h>

struct hp
{
    int index;
    int k;
    int v;
};

struct queue
{
    int** heap;
    int cap;
    int count;
};

void InitPriorityQueue(struct queue* q, int n)
{
    q->heap=(struct hp*)malloc(sizeof(struct hp)*n);
    q->cap=n;
    q->count=0;
}

int Maximum(struct queue* q)
{
    return q->heap[0];
}

bool QueueEmpty(struct queue* q)
{
    return q->count==0 ? true : false;
}

void Insert(struct queue* q, int ptr)
{
    int i=q->count;
    q->count=i+1;
    q->heap[i]->
}

int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);

    return 0;
}
