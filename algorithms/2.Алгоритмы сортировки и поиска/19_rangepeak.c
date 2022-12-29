#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <string.h>
#include <stdlib.h>

int PEAK(long int* A, int l, int r, int n)
{
    int count=0;
    for(int i=l; i<r+1; ++i)
    {
        if (l==r)
        {
            if (l==0)
                {if(A[i]>=A[i+1])
            {
              count+=1;

            }}
            else if (l==n-1)
                {
                    if (A[i]>=A[i-1]) count+=1;
                }
            else if (A[i]>=A[i-1] and A[i]>=A[i+1])
                count+=1;
        }
        else if(i==0)
            {
                if(A[i]>=A[i+1])
                count+=1;
            }
        else if(i==n-1)
                {
                    if(A[i]>=A[i-1])
                    count+=1;
                }
                else
                {
                    if(A[i]>=A[i-1] and A[i]>=A[i+1])
                    count+=1;
                }
    }
    return count;
}

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
            v=first+second;
        }
    }
    return v;
}

void build(long int* A, int a, int b, long int* T, int max, int n)
{
    if(a==b)
        T[max]=PEAK(A, a, b, n);
    else
    {
        int m=(a+b)/2;
        build(A, a, m, T, 2*max+1, n);
        build(A, m+1, b, T, 2*max+2, n);
        long int first=T[2*max+1];
        long int second=T[2*max+2];
        T[max]=first+second;
    }
}

void update(int i, int a, int b, long int* T, int max, long int* A, int n);

void segmenttree_update(int i, long int v, int n, long int* T, long int* A)
{
    A[i]=v;
    if(i==0 or i==n-1)
        update(i, 0, n-1, T, 0, A, n);
    if(i==0)
    {
        update(i+1, 0, n-1, T, 0, A, n);
        update(i, 0, n-1, T, 0, A, n);
    }
    if(i==n-1)
    {
        update(i-1, 0, n-1, T, 0, A, n);
        update(i, 0, n-1, T, 0, A, n);
    }
    if(i!=0 and i!=n-1)
    {
        update(i-1, 0, n-1, T, 0, A, n);
        update(i+1, 0, n-1, T, 0, A, n);
        update(i, 0, n-1, T, 0, A, n);
    }
}

void update(int i, int a, int b, long int* T, int max, long int* A, int n)
{
    if(a==b)
        T[max]=PEAK(A, a, b, n);
    else
    {
        int m=(a+b)/2;
        if(i<=m)
            update(i, a, m, T, 2*max+1, A, n);
        else
            update(i, m+1, b, T, 2*max+2, A, n);
        long int first=T[2*max+1];
        long int second=T[2*max+2];
        T[max]=first+second;
    }
}

int main(int argc, char* argv[])
 {
    int n;
    int ii=0;
    int li=0;
    int ri=0;
    long int l=0;
    long int r=0;
    char c;
    scanf("%d", &n);
    long int A[n];
    long int* T=(long int*)malloc(sizeof(long int)*n*10);
    char LL[10];
    char RR[10];
    char str[15];
    char musor;//забирает энтер или пробел
    scanf("%c", &musor);
    for(long int i=0; i<n; ++i)
    {
        scanf("%c", &c);
        while(c!=' ' and c!='\n')
        {
            str[ii]=c;
            ii+=1;
            scanf("%c", &c);
        }
        str[ii]='\0';
        A[i]=atol(str);
        ii=0;

    }
    build(A, 0, n-1, T, 0, n);

    while(true)
    {
        scanf("%c", &c);//P или U
        if(c=='P')
        {
            scanf("%c", &c);//E
            scanf("%c", &c);//A
            scanf("%c", &c);//K
            scanf("%c", &c);//' '
            scanf("%c", &c);
            while(c!=' ')
            {
                LL[li]=c;
                li+=1;
                scanf("%c", &c);
            }
            LL[li]='\0';

            scanf("%c", &c);
            while(c!='\n')
            {
                RR[ri]=c;
                ri+=1;
                scanf("%c", &c);
            }
            RR[ri]='\0';
            ri=li=0;
            l=atol(LL);
            r=atol(RR);
            printf("%ld\n", segmenttree_query(T, n, l, r));
        }
        else if(c=='U')
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
            scanf("%c", &c);
            while(c!='\n')
            {
                RR[ri]=c;
                ri+=1;
                scanf("%c", &c);
            }
            RR[ri]='\0';
            ri=li=0;
            l=atol(LL);
            r=atol(RR);
            segmenttree_update(l, r, n, T, A);//l=i, r=v
        }
        else break;
    }
    free(T);
    return 0;
 }
