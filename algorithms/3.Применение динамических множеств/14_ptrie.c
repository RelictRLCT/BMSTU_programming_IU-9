#include <stdio.h>
#include <iso646.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

struct ver{
    long int v;
    struct ver* parent;
    struct ver** arcs;
};

void init_trie(struct ver* t)
{
    t->v=0;
    t->parent=NULL;
    t->arcs=(struct ver**)malloc(sizeof(struct ver*)*26);
    for(int i=0; i<26; ++i)
        t->arcs[i]=NULL;
}

void liquid_bor(struct ver* t);

bool mapempty(struct ver t)
{
    if(t.v!=0)
        return false;
    long int i=0;
    while(i<26)
    {
        if(t.arcs[i]!=NULL)
            return false;
        i+=1;
    }
    return true;
}

void minus(struct ver* x)
{
    int i=0;
    if(x!=NULL)
    {
        while(i<26)
        {
            minus(x->arcs[i]);
            i+=1;
        }
        x->v-=1;
    }
}


int Descend(struct ver* t, char* k, struct ver* x, int metka_i_d)
{
    x=t;
    int i=0;
    int len=strlen(k);
    while(i<len)
    {
        struct ver* par=x;
        struct ver* y=x->arcs[(int)(k[i]-'a')];
        if(y==NULL)
            break;
        x=y;
        if(metka_i_d==1)
            x->v+=1;
        else
        {
            x->v-=1;
            if(x->v==0)
            {
                liquid_bor(x);
                par->arcs[(int)(k[i]-'a')]=NULL;
                break;
            }
        }
        i+=1;
    }
    return i;
}

void Insert(struct ver* t, char* k)
{
    struct ver* x=t;
    int i=0;
    int len=strlen(k);
    while(i<len)
    {
        if(NULL==x->arcs[(int)(k[i]-'a')])
        {
            struct ver* y=(struct ver*)malloc(sizeof(struct ver));
            init_trie(y);
            y->parent=x;
            x->arcs[(int)(k[i]-'a')]=y;
        }
        x=x->arcs[(int)(k[i]-'a')];
        i+=1;
        x->v+=1;
    }
    if(x->v>1)
    {
        while(x!=NULL)
        {
            x->v-=1;
            x=x->parent;
        }
    }
}

void Delete(struct ver* t, char* k)
{
    struct ver* x=t;
    int i=Descend(t, k, x, 0);
    struct ver* y=x;
    int len=strlen(k);
    while(x->parent!=NULL and x->v==0)
    {
        int j=0;
        while(j<26 and x->arcs[j]==NULL)
        {
            j+=1;
        }
        if(j<26)
            break;
        struct ver* y=x->parent;
        i-=1;
        y->arcs[(int)(k[i]-'a')]=NULL;
        if(x->v==0)
        {
            x->parent->arcs[(int)(k[i]-'a')]=NULL;
            liquid_bor(x);
            break;
        }
        x=y;
        x->v-=1;
    }
}

long int Prefix(struct ver* t, char* p)
{
    struct ver* x=t;
    int len=strlen(p);
    int i=0;
    while(i<len and x!=NULL)
    {
        x=x->arcs[(int)(p[i]-'a')];
        i+=1;
        if(x==NULL)
            break;
    }
    if(x!=NULL)
        return x->v;
    return 0;
}

void liquid_bor(struct ver* t)
{
    int i=0;
    if(t!=NULL)
    {
        while(i<26)
        {
            liquid_bor(t->arcs[i]);
            i+=1;
        }
        free(t->arcs);
        free(t);
    }
}

int main(int argc, char* argv[])
{
    struct ver* t=(struct ver*)malloc(sizeof(struct ver));
    init_trie(t);
    char s[20];
    while(true)
    {
        scanf("%s", s);
        if(s[0]=='I')
        {
            char str[100000];
            scanf("%s", str);
            Insert(t, str);
        }
        else if(s[0]=='D')
        {
            char str[100000];
            scanf("%s", str);
            Delete(t, str);
        }
        else if(s[0]=='P')
        {
            char str[100000];
            scanf("%s", str);
            printf("%ld\n", Prefix(t, str));
        }
        else break;
    }
    liquid_bor(t);
    return 0;
}
