#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdlib.h>
#include <string.h>

struct Elem {
    struct Elem *next;
    char *word;
};

int ListLength(struct Elem* l)
{
    int len=0;
    struct Elem* x=l;
    while(x!=NULL)
    {
        len+=1;
        x=x->next;
    }
    return len;
}

struct Elem *bsort(struct Elem *list)
{
    int t=ListLength(list)-1;
    while(t>0)
    {
        struct Elem* tmp=list;
        int bound = t;
        t=0;
        int i=0;
        while(i<bound)
        {
            if(strlen(tmp->next->word)<strlen(tmp->word))
            {
                char* tmpword=tmp->word;
                tmp->word=tmp->next->word;
                tmp->next->word=tmpword;
                t=i;
            }
            i+=1;
            tmp=tmp->next;
        }
    }
    return list;
}

int main(int argc, char* argv[])
{
    struct Elem* l=(struct Elem*)malloc(sizeof(struct Elem)), *list;
    l->next=NULL;
    list=l;
    char c=' ';
    scanf("%c", &c);
    char* wordd=(char*)malloc(1);
    int lenn=1;
    while(c!=' ' and c!='\n')
    {
        wordd[lenn-1]=c;
        lenn+=1;
        wordd=(char*)realloc(wordd, lenn);
        wordd[lenn-1]='\0';
        scanf("%c", &c);
    }
    list->word=wordd;
    int guard_malloc=1;
    char word[1001];
    while(c!='\n')
    {
        struct Elem* element=(struct Elem*)malloc(sizeof(struct Elem));
        scanf("%c", &c);
        int len=1;
        while(c!=' ' and c!='\n')
        {
            word[len-1]=c;
            len+=1;
            word[len-1]='\0';
            scanf("%c", &c);
        }
        if(len!=1)
        {
            element->word=(char*)malloc(strlen(word)+1);
            strcpy(element->word, word);
            list->next=element;
            list=list->next;
        }
        else free(element);
    }
    list->next=NULL;
    bsort(l);
    struct Elem* freee=l;
    struct Elem* freee2=l;
    int ccc=0;
    while(l!=NULL)
    {
        freee=l;
        printf("%s ", l->word);
        l=l->next;
        free(freee->word);
        free(freee);
        ccc+=1;
    }
    free(l);
    return 0;
}
