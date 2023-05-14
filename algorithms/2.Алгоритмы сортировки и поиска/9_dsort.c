#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <string.h>
#include <stdlib.h>

void Dissort(char* str, long int n)
{
    long int chars[26]={0};
    int j=0;
    while(j<n)
    {
        chars[(int)(str[j]-'a')]+=1;
        j+=1;
    }
    j=0;
    while(j<26)
    {
        while(chars[j]!=0)
        {
            printf("%c", (char)(j+(int)'a'));
            chars[j]-=1;
        }
        j+=1;
    }
}

int main(int argc, char* argv[])
{
    char str[1000001];
    gets(str);
    Dissort(str, strlen(str));

    return 0;
}
