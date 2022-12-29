#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <string.h>
#include <stdlib.h>

long int query(long int* T, int l, int r, int a, int b, int max);

long int segmenttree_query(long int* T, int n, int l, int r)
{
    return query(T, l, r, 0, n-1, 0);
}

long int query(long int* T, int l, int r, int a, int b, int max)
{
    long int v;
    if(l==a and r==b)
        v=T[max];
    else
    {
        int m=(a+b)/2;
        if(r<=m)
            v=query(T, l, r, a, m, 2*max+1);
        else if(l>m)
            v=query(T, l, r, m+1, b, 2*max+2);
        else
        {
            long int first=query(T, l, m, a, m, 2*max+1);
            long int second=query(T, m+1, r, m+1, b, 2*max+2);
            v=first>second ? first : second;
        }
    }
    return v;
}

void build(long int* A, int a, int b, long int* T, int max)
{
    if(a==b)
        T[max]=A[a];
    else
    {
        int m=(a+b)/2;
        build(A, a, m, T, 2*max+1);
        build(A, m+1, b, T, 2*max+2);
        long int first=T[2*max+1];
        long int second=T[2*max+2];
        T[max]=first>second ? first : second;
    }
}

void update(int i, long int v, int a, int b, long int* T, int max);

void segmenttree_update(int i, long int v, int n, long int* T)
{
    update(i, v, 0, n-1, T, 0);
}

void update(int i, long int v, int a, int b, long int* T, int max)
{
    if(a==b)
        T[max]=v;
    else
    {
        int m=(a+b)/2;
        if(i<=m)
            update(i, v, a, m, T, 2*max+1);
        else
            update(i, v, m+1, b, T, 2*max+2);
        long int first=T[2*max+1];
        long int second=T[2*max+2];
        T[max]=first>second ? first : second;
    }
}

int main(int argc, char* argv[])
{
    int n;
    int m;
    int r;
    int l;
    int ri=0;
    int li=0;
    int ii=0;
    long int x;
    char LL[20];
    char RR[20];
    scanf("%d", &n);
    long int A[n];
    long int* T=(long int*)malloc(sizeof(long int)*n*10);
    char musor;
    char c;
    char str[20];
    for(int i=0; i<n; ++i)
    {
        scanf("%ld", &A[i]);
    }
    build(A, 0, n-1, T, 0);
    scanf("%c", &musor);
    while(true)
    {
        scanf("%c", &c);
        if(c=='U')
        {
            scanf("%c", &c);//P
            scanf("%c", &c);//D
            scanf("%c", &c);//' '
            scanf("%c", &c);
            while(c!=' ')
            {
                LL[li]=c;
                li+=1;
                scanf("%c", &c);
            }
            LL[li]='\0';
            l=atoi(LL);

            scanf("%c", &c);
            while(c!=' ' and c!='\n')
            {
                RR[ri]=c;
                ri+=1;
                scanf("%c", &c);
            }
            RR[ri]='\0';
            x=atol(RR);
            ri=li=0;
            segmenttree_update(l, x, n, T);
        }
        else if(c=='M')
        {
            scanf("%c", &c);//A
            scanf("%c", &c);//X
            scanf("%c", &c);//' '
            scanf("%c", &c);
            while(c!=' ')
            {
                LL[li]=c;
                li+=1;
                scanf("%c", &c);
            }
            LL[li]='\0';
            l=atoi(LL);
            scanf("%c", &c);
            while(c!=' ' and c!='\n')
            {
                RR[ri]=c;
                ri+=1;
                scanf("%c", &c);
            }
            RR[ri]='\0';
            r=atoi(RR);
            ri=li=0;
            printf("%ld\n", segmenttree_query(T, n, l, r));
        }
        else break;
    }
    free(T);
    return 0;
}
