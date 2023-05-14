#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <string.h>
#include <stdlib.h>

char *fibstr(int n)
{
    char* str;
    char** strs=(char**)malloc(sizeof(char*)*2);
    strs[0]="a";
    strs[1]="b";
    for (int i=2; i<n; ++i)
    {
        strs=(char**)realloc(strs, sizeof(char*)*(i+1));
        strs[i]=(char*)malloc(strlen(strs[i-2]) + strlen(strs[i-1]) + 1);
        char* tmp=(char*)malloc(strlen(strs[i-1])+strlen(strs[i-2]) +1);
        memcpy(tmp, strs[i-2], strlen(strs[i-2]));
        memcpy(tmp+strlen(strs[i-2]), strs[i-1], strlen(strs[i-1])+1);
        strcpy(strs[i], tmp);

        free(tmp);
    }
    if (n==1)
    {
        str=(char*)malloc(2);
        str[0]='a';
        str[1]='\0';
    }
    else if (n==2)
    {
        str=(char*)malloc(2);
        str[0]='b';
        str[1]='\0';
    }
    else
    {
        str=(char*)malloc((strlen(strs[n-2])+strlen(strs[n-1])+1));
        strcpy(str, strs[n-1]);
    }
    for(int i=2; i<n; ++i)
        free(strs[i]);
    free(strs);
    return str;
}

int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);
    char* str=fibstr(n);
    printf("%s\n", str);
    free(str);
    return 0;
}
