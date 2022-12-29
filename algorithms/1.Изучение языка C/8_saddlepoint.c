#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>

int main(int argc, char* argv[])
{
    int n, m, x;
    bool flag=false;
    scanf("%d%d", &n, &m);
    int prohod_guard[m];
    int max[n], min[m];
    int A[n][m];
    int STR[10];
    for(int i=0; i<n; ++i)
        STR[i]=-1;
    for(int i=n; i<10; ++i) //массив STR заполнен -1 и -2, -1 => элемент есть
        STR[i]=-2;
    for(int i=0; i<n; ++i)
        max[i]=-9999;
    for(int i=0; i<m; ++i)
        {
            min[i]=10000;
            prohod_guard[i]=-9999;
        }
    for(int i=0; i<n; ++i)
        for(int j=0; j<m; ++j)
        {
            scanf("%d", &x);
            if (x > max[i])
            {
                max[i]=x;
                STR[i]=j;
            }
            A[i][j]=x;
        }

    for(int i=0; i<m; ++i)
    {
        for(int j=0; j<n; ++j)
        {
            if(STR[i]>=0 and STR[i]<m and (prohod_guard[STR[i]]==-9999))
            {
                if(STR[i]!=-2)
                {
                    if(A[j][STR[i]] < min[STR[i]])
                        min[STR[i]]=A[j][STR[i]];
                }
            }
            else min[STR[i]]=prohod_guard[STR[i]];
        }
        if(STR[i]!=-2) prohod_guard[STR[i]]=min[STR[i]];
    }

    for(int i=0; i<n; ++i)
    {
        for(int j=0; j<m; ++j)
        if(max[i]==min[STR[i]])
        {
            printf("%d %d", i, STR[i]);
            flag=true;
            break;
        }
        if(flag==true)
            break;
    }
    if(flag==false)
        printf("none\n");

    return 0;
}
