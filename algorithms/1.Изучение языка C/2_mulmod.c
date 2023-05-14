#include <stdio.h>
#include <iso646.h>
#include <stdbool.h>

int main(int argc, char* argv[])
{
        unsigned long long int a, b, m;
        scanf("%lld %lld %lld", &a, &b, &m);
        int B2[64]={0};
        int i=-1;
        while(b>0)
        {
            i+=1;
            B2[i]= (b%2) ;
            b/=2;

        }
        int B22[i+1];
        int j=-1;
     for(; i>=0; --i)
        {
             j+=1;
          B22[j]=B2[i];
        }

         unsigned long long int res=0;

         for(int i=0; i<j; ++i)
            {
                res=((res+a*B22[i])%m)*2;
            }
            res=res+(B22[j]*a)%m;
            res%=m;
            printf("%lld", res);

        return 0;
}
