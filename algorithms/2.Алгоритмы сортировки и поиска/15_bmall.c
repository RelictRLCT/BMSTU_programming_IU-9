#include <stdio.h>
#include <iso646.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))
void Delta1(char* S, int size, int* d1)
{
    int a=0;
    int len=strlen(S);
    while(a<size)
    {
        d1[a]=len;
        a+=1;
    }
    int j=0;
    while(j<len)
    {
        d1[(int)S[j]-33]=len-j-1;
        j+=1;
    }
}

void SimpleBMSubst(char* S, int size, char* T)
{
    int i=0;
    int d1[size];
    Delta1(S, size, d1);
    int lens=strlen(S);
    int lent=strlen(T);
    int k=lens-1;
    while(k<lent)
    {
        i=lens-1;
        while(k>=0 and i>=0 and k<lent and i<lens and T[k]==S[i])
        {
            if(i==0)
            {
                printf("%d ", k);
            }
            i-=1;
            k-=1;
        }
        if (k<lent and k>=0)
        {
            k+=MAX(d1[(int)T[k]-33], lens-i);
        }
        else if (k<0)
                k+=lens-i;
    }
    k=lent;
}

int main(int argc, char* argv[])
{
    char S[strlen(argv[1])+1];
    strcpy(S, argv[1]);
    char T[strlen(argv[2])+1];
    strcpy(T, argv[2]);
    SimpleBMSubst(S, 94, T);
    return 0;
}
