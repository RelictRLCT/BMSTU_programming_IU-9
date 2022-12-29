#include <stdio.h>
#include <stdlib.h>
#include <iso646.h>
#include <stdbool.h>

struct ver{
    long long int k;
    char* v;
    struct ver* parent;
    struct ver* left;
    struct ver* right;
    int count;
};

struct tree{
    struct ver* root;
};

void init_tree(struct tree* t)
{
    t->root=NULL;
}

void Insert(struct tree* t, long long int k, char* v)
{
    struct ver* y=(struct ver*)malloc(sizeof(struct ver));
    y->k=k;
    y->v=v;
    y->count=0;
    y->parent=y->left=y->right=NULL;
    if(t->root==NULL)
        t->root=y;
    else
    {
        struct ver* x=t->root;
        while(true)
        {
            x->count+=1;
            if(k<x->k)
            {
                if(x->left==NULL)
                {
                    x->left=y;
                    y->parent=x;
                    break;
                }
                x=x->left;
            }
            else
            {
                if(x->right==NULL)
                {
                    x->right=y;
                    y->parent=x;
                    break;
                }
                x=x->right;
            }
        }
    }
}

void replace_node(struct tree* t, struct ver* x, struct ver* y)
{
    if(x==t->root)
    {
        t->root=y;
        if(y!=NULL)
            y->parent=NULL;
    }
    else
    {
        struct ver* p=x->parent;
        if(y!=NULL)
            y->parent=p;
        if(p->left==x)
            p->left=y;
        else
            p->right=y;
    }
}

struct ver* Minimum(struct ver* x)
{
    if(x!=NULL)
    {
        while(x->left!=NULL)
        {
            x->count-=1;
            x=x->left;
        }
    }
    return x;
}

struct ver* Succ(struct ver* x)
{
    struct ver* y;
    if(x->right!=NULL)
        y=Minimum(x->right);
    else
    {
        y=x->parent;
        while(y!=NULL and x==y->right)
        {
            x=y;
            y=y->parent;
        }
    }
    return y;
}

struct ver* Descend(struct tree* t, long long int k, int metka_del)
{
    struct ver* x=t->root;
    while(x!=NULL and x->k!=k)
    {
        if(metka_del==1)
            x->count-=1;
        if(k<x->k)
            x=x->left;
        else
            x=x->right;
    }
    return x;
}

char* Lookup(struct tree* t, long long int k)
{
    struct ver* x=Descend(t, k, 0);
    return x->v;
}

void Delete(struct tree* t, long long int k)
{
    struct ver* x=Descend(t, k, 1);
    if(x->left==NULL and x->right==NULL)
        replace_node(t, x, NULL);
    else if(x->left==NULL)
        replace_node(t, x, x->right);
    else if(x->right==NULL)
        replace_node(t, x, x->left);
    else
    {
        struct ver* y=Succ(x);
        replace_node(t, y, y->right);
        x->left->parent=y;
        y->left=x->left;
        if(x->right!=NULL)
            x->right->parent=y;
        y->right=x->right;
        y->count=x->count-1;//
        replace_node(t, x, y);
    }
    free(x->v);
    free(x);
}

char* Search(struct tree* t, long long int x)
{
    struct ver* y=t->root;
    while(x>=0)
    {
        if(y->left!=NULL and y->right!=NULL and x==y->left->count+1
           and x==y->right->count+1)
            break;
        if(y->left!=NULL and x<=y->left->count+1)
            {
                if(x==y->left->count+1)
                    break;
                else
                    y=y->left;
            }
        else if(y->right!=NULL and x!=0)
            {
                if(y->left!=NULL and x>y->left->count+1)
                    x-=(y->left->count+2);
                else
                    x-=1;
                y=y->right;
            }
        else break;
    }
    return y->v;
}

void liquid_tree(struct ver* t)
{
    if(t!=NULL)
    {
        liquid_tree(t->left);
        liquid_tree(t->right);
        free(t->v);
        free(t);
    }
}

int main(int argc, char* argv[])
{
    struct tree t;
    init_tree(&t);
    char s[35];
    while(true)
    {
        scanf("%s", s);
        if(s[0]=='I')
        {
            long long int k;
            char* str=(char*)malloc(10);
            scanf("%lld %s", &k, str);
            Insert(&t, k, str);
        }
        else if(s[0]=='L')
        {
            long long int k;
            scanf("%lld", &k);
            printf("%s\n", Lookup(&t, k));
        }
        else if(s[0]=='D')
        {
            long long int k;
            scanf("%lld", &k);
            Delete(&t, k);
        }
        else if(s[0]=='S')
        {
            long long int x;
            scanf("%lld", &x);
            printf("%s\n", Search(&t, x));
        }
        else break;
    }
    liquid_tree(t.root);
    return 0;
}
