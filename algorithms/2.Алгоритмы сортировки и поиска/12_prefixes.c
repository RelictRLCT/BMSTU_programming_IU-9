#include <stdio.h>
#include <stdbool.h>
#include <iso646.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
    bool period=true;
    int n=0;
    int k=0;
    char* pref=(char*)malloc(1);
    char* prefpref=(char*)malloc(1);
    char S[strlen(argv[1])+1];
    strcpy(S, argv[1]);
    for(int i=1; i<strlen(S); ++i)
    {
        n=k=0;
        int counter=1;
        for(int ii=0; ii<i+1; ++ii)
        {
            pref[ii]=S[ii];
            pref=(char*)realloc(pref, counter+1);
            pref[counter]='\0';
            counter+=1;
        }

        for(int ii=0; ii<strlen(pref)/2; ++ii)
        {
            period=true;
            int iii=0;
            counter=1;
            while(iii<ii+1)
            {
                prefpref[iii]=pref[iii];
                prefpref=(char*)realloc(prefpref, counter+1);
                prefpref[counter]='\0';
                iii+=1;
                counter+=1;
            }
            for(int iiii=0; iiii<strlen(pref); iiii+=strlen(prefpref))
            {
                int iterpref=0;
                for(int iter=iiii; iter<iiii+strlen(prefpref); ++iter)
                {
                    if(prefpref[iterpref]!=pref[iter])
                    {
                        period=false;
                        break;
                    }
                    iterpref+=1;
                }
                if(period==false)
                    break;
            }
            if(period==false)
                continue;
            else
            {
                n=strlen(pref);
                if(strlen(pref)/strlen(prefpref)>k)
                {
                    k=strlen(pref)/strlen(prefpref);
                    printf("%d %d\n", n, k);
                }
            }
        }
    }
    free(pref);
    free(prefpref);
}
