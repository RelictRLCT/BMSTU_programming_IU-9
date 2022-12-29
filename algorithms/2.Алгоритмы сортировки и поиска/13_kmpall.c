#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <string.h>
#include <stdlib.h>

void Prefix(char* S, int* Pi)
{
    int t;
    Pi[0]=t=0;
    int i=1;
    int len=strlen(S);
    while(i<len)
    {
        while(t>0 and S[t]!=S[i])
            t=Pi[t-1];
        if(S[t]==S[i])
            t+=1;
        Pi[i]=t;
        i+=1;
    }
}

void KMP(char* S, char* T)
{
    int* Pi=(int*)malloc(strlen(S)*sizeof(int));
    Prefix(S, Pi);
    int q=0;
    int k=0;
    int lens=strlen(S);
    int lent=strlen(T);
    while(k<lent)
    {
        while(q>0 and S[q]!=T[k])
            q=Pi[q-1];
        if(S[q]==T[k])
            q+=1;
        if(q==lens)
        {
            printf("%d ", k-q+1);
        }
         k+=1;
    }
    free(Pi);
}

int main(int argc, char* argv[])
{
    int i=0;
    char c;
    char* S=(char*)malloc(strlen(argv[1])+1);    //ввод с консоли
    strcpy(S, argv[1]);
    char* T=(char*)malloc(strlen(argv[2])+1);
    strcpy(T, argv[2]);

/*
  char* S=(char*)malloc(1);                      //ввод с клавиатуры
   scanf("%c", &c);
   while(c!='\0'and c!='\n')
   {
       S[i]=c;
       i+=1;
       S=(char*)realloc(S, i+1);
       scanf("%c", &c);
   }
   S[i]='\0';
    i=0;
   char* T=(char*)malloc(1);
   scanf("%c", &c);
   while(c!='\0'and c!='\n')
   {
       T[i]=c;
       i+=1;
       T=(char*)realloc(T, i+1);
       scanf("%c", &c);
   }
   T[i]='\0';
*/
    KMP(S, T);
    free(T);
    free(S);
    return 0;
}
