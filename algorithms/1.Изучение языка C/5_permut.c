#include <stdio.h>
#include <iso646.h>
#define N 8

int main(int argc, char* argv[])
{
    int a[N], b[N];
    for(int i=0; i<N; ++i)
    {
        scanf("%d", &a[i]);
    }
    for(int i=0; i<N; ++i)
    {
        scanf("%d", &b[i]);
    }

    for(int i=0; i<N; ++i)
        for(int j=0; j<N; ++j)
        {
            if(a[i]==b[j])
            {
                a[i]=-1000000;
                b[j]=-1000000;
                break;
            }
        }
    int k=0;
    for(int i=0; i<N; ++i)
        {
            if ((a[i]== -1000000) and (b[i]== -1000000))
                k+=1;
        }
    if (k==N)
        printf("yes");
    else printf("no");

    return 0;
}
