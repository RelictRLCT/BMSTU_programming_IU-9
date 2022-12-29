#include <stdio.h>
#include <iso646.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

int words(char* s, char*** words)
{
    char** word=(char**)malloc(sizeof(char*));
    int lens=strlen(s);
    int i=0;
    int count=0;
    word[0]=(char*)malloc(1);
    while(s[i]!='\0')
    {
        if(count>0 and s[i]!=' ')
            {
                word=(char**)realloc(word, (sizeof(char*))*(count+1));
                word[count]=(char*)malloc(1);
            }
        int alloc=1;
        int iter=0;
        while(s[i]!=' ' and s[i]!='\0')
        {
            word[count][iter]=s[i];
            iter+=1;
            alloc+=1;
            word[count]=(char*)realloc(word[count], alloc);
            i+=1;
            word[count][iter]='\0';
        }
        if(s[i-1]!=' ')
            count+=1;
        if(s[i]=='\0')
            break;
        i+=1;
    }
    *words=word;
    return s[0]==' ' ? count-1 : count;
}

void csort(char *src, char *dest)
{
    char** word;
    int lenw=words(src, &word);
    int count[lenw];
    for(int i=0; i<lenw; ++i)
    {
        count[i]=0;
    }
    int j=0;
    while(j<lenw-1)
    {
        int lenw1=strlen(word[j]);
        int i=j+1;
        while(i<lenw)
        {
            int lenw2=strlen(word[i]);
            if(lenw2<lenw1)
                count[j]+=1;
            else
                count[i]+=1;
            i+=1;
        }
        j+=1;
    }

    int len=0;
    int lenpred=0;
    for(int i=0; i<lenw; ++i)
    {
        int cc=0;
        while(count[cc]!=i)
            cc+=1;
        strcpy(dest+len, word[cc]);
        len+=strlen(word[cc]);
        if (i!=lenw-1)
        {
            dest[len]=' ';
            dest[len+1]='\0';
        }
        lenpred=strlen(word[cc]);
        len+=1;
    }
    for(int i=0; i<lenw; ++i)
        free(word[i]);
    free(word);
}

int main(int argc, char* argv)
{
    char str[1000];
    gets(str);
    int lend=0;
    int lens=strlen(str);
    for(int i=0; i<lens; ++i)
    {
        if(str[i]!=' ')
            lend+=1;
        else if(str[i-1]!=' ')
                lend+=1;
    }
    lend+=1;//'\0'
    char dest[lend];
    dest[0]='\0';
    csort(str, dest);
    printf("%s", dest);
    return 0;
}
