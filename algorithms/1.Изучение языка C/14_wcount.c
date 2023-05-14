#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <string.h>
#include <stdlib.h>

int wcount(char *s)
{
    if (s[0]=='\0')
        return 0;
    char c;
    c=s[0];
    int i=0;
    int count=0;
    while(c!='\0' and i<1000)
    {
        if(c==' ' and i!=0 and s[i-1]!=' ')
            count+=1;
        i+=1;
        c=s[i];
    }
    if (s[i-1]!=' ')
        count+=1;
    return count;
}

int main(int argc, char* argv[])
{
    char str[1000];
    gets(str);
    if (str[0] == '\0')
        printf("%d", 0);
    else
    printf("%d\n", wcount(str));
    return 0;
}
