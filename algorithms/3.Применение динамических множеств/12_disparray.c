#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdlib.h>

struct List {
        long int k;
        long int v;
        struct List *next;
    };

struct HashTable {
    size_t elem_count; // количество элементов
    size_t table_size; // размер таблицы
    struct List **table;
};

long int list_search(struct List* l, long int i)
{
    struct List* x=l;
    while(x!=NULL and x->k!=i)
        x=x->next;
    if(x!=NULL)
    return x->v;
    else return 0;
}

long int lookup(struct HashTable t, long int i)
{
    long int p=list_search(t.table[i % t.table_size], i);
    return p;
}

void hash_init(struct HashTable *t, size_t m) {
    t->elem_count = 0;
    t->table_size = m;
    t->table = malloc(sizeof(*t->table) * m);
    for (size_t i = 0; i < m; ++i) {
        t->table[i] = NULL;
    }
}

void hash_destroy(struct HashTable *t) {
    for (size_t i = 0; i < t->table_size; ++i) {
        struct List *bucket = t->table[i];
        while (bucket != NULL) {
            struct List *head = bucket;
            bucket = bucket->next;
            free(head);
        }
    }
    free(t->table);
}

void hash_insert(struct HashTable *t, long int k, long int v) {
    long int h = k % t->table_size;
    struct List *bucket = t->table[h];
    while (bucket != NULL and bucket->k != k) bucket = bucket->next;
    bucket =(struct List*)malloc(sizeof(*bucket));
    bucket->k = k;
    bucket->v = v;
    bucket->next = t->table[h];
    t->table[h] = bucket;
    t->elem_count+=1;
}

int main(int argc, char* argv[])
{
    struct HashTable tab;
    long int m;
    scanf("%ld", &m);
    hash_init(&tab, m);
    char musor;
    scanf("%c", &musor);
    char s[20];
    while(true)
    {
        scanf("%s", s);
        if(s[0]=='A' and s[1]=='S')
        {
            long int i;
            long int v;
            scanf("%ld", &i);
            scanf("%ld", &v);
            hash_insert(&tab, i, v);
        }
        else if(s[0]=='A')
        {
            long int i;
            scanf("%ld", &i);
            printf("%ld\n", lookup(tab, i));
        }
        else break;
    }
    hash_destroy(&tab);
    return 0;
}
