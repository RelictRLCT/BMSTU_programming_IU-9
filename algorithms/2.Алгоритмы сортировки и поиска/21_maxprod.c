#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <string.h>
#include <stdlib.h>

void Kadane(double* str, int n, int* l, int* r)
{
    *l=0;
    *r=0;
    double maxpr=str[0];
    int start=0;
    double pr=1;
    int i=0;
    while(i<n)
    {
        pr*=str[i];
        if(pr>maxpr)
        {
            maxpr=pr;
            *l=start;
            *r=i;
        }
        i+=1;
        if(pr<1)
        {
            pr=1.0;
            start=i;
        }
    }
}

int main(int argc, char* argv[])
{
    int n;
    int l=0;
    int r=0;
    char chislitel[100];
    char znamenatel[100];
    scanf("%d", &n);
    double str[n];
    char c;
    int i=0;
    int ii=0;
    char musor;
    scanf("%c", &musor);
    for(int j=0; j<n; ++j)
    {
        scanf("%c", &c);
        while(c!='/')
        {
            chislitel[i]=c;
            i+=1;
            scanf("%c", &c);
        }
            chislitel[i]='\0';
        scanf("%c", &c);

        while(c!=' ' and c!='\n')
        {
            znamenatel[ii]=c;
            ii+=1;
            scanf("%c", &c);
        }
        znamenatel[ii]='\0';
        i=ii=0;
        str[j]=(double)(atoi(chislitel))/(atoi(znamenatel));
    }

    Kadane(str, n, &l, &r);
    printf("%d %d", l, r);
    return 0;
}
