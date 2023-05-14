#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdlib.h>

struct stack
{
    long int* data;
    int cap;
    int top;
};

void InitStack(struct stack* s, int n)
{
    s->data=(long int*)malloc(sizeof(long int)*n);
    s->cap=n;
    s->top=0;
}

bool StackEmpty(struct stack s)
{
    return s.top==0 ? true : false;
}

void Push(struct stack* s, long int x)
{
    s->data[s->top]=x;
    s->top+=1;
}

long int Pop(struct stack* s)
{
    s->top-=1;
    return s->data[s->top];
}

void CONST(struct stack* s, long int x)
{
    Push(s, x);
}

void ADD(struct stack* s)
{
    long int a;
    long int b;
    a=Pop(s);
    b=Pop(s);
    Push(s, a+b);
}

void SUB(struct stack* s)
{
    long int a;
    long int b;
    a=Pop(s);
    b=Pop(s);
    Push(s, a-b);
}

void SWAP(struct stack* s)
{
    long int a;
    long int b;
    a=Pop(s);
    b=Pop(s);
    Push(s, a);
    Push(s, b);
}

void MUL(struct stack* s)
{
    long int a;
    long int b;
    a=Pop(s);
    b=Pop(s);
    Push(s, a*b);
}

void MAX(struct stack* s)
{
    long int a;
    long int b;
    a=Pop(s);
    b=Pop(s);
    Push(s, a>b ? a : b);
}

void MIN(struct stack* s)
{
    long int a;
    long int b;
    a=Pop(s);
    b=Pop(s);
    Push(s, a>b ? b : a);
}

void DIV(struct stack* s)
{
    long int a;
    long int b;
    a=Pop(s);
    b=Pop(s);
    Push(s, a/b);
}

void DUP(struct stack* s)
{
    long int a;
    a=Pop(s);
    Push(s, a);
    Push(s, a);
}

void NEG(struct stack* s)
{
    long int a;
    a=Pop(s);
    Push(s, -a);
}

int main(int argc, char* argv[])
{
    struct stack sta;
    int n;
    int ix=0;
    char X[15];
    InitStack(&sta, 500000);
    char s[30];
    while(true)
    {
        scanf("%s", s);
        if(s[0]=='C')
        {
            long int x;
            char c;
            scanf("%c", &c);
            while(c!='\n')
            {
                X[ix]=c;
                ix+=1;
                scanf("%c", &c);
            }
            X[ix]='\0';
            ix=0;
            x=atoi(X);
            CONST(&sta, x);
        }
        else if(s[0]=='A')
        {
            ADD(&sta);
        }
        else if(s[0]=='S')
        {
            if(s[1]=='U')
            {
                SUB(&sta);
            }
            else
            {
                SWAP(&sta);
            }
        }
        else if(s[0]=='M')
        {
            if(s[1]=='U')
            {
                MUL(&sta);
            }
            else if(s[1]=='A')
            {
                MAX(&sta);
            }
            else
            {
                MIN(&sta);
            }
        }
        else if(s[0]=='D')
        {
            if(s[1]=='I')
            {
                DIV(&sta);
            }
            else
            {
                DUP(&sta);
            }
        }
        else if(s[0]=='N')
        {
            NEG(&sta);
        }
        else break;
    }
    printf("%ld\n", sta.data[sta.top-1]);
    free(sta.data);
    return 0;
}
