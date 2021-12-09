#include<stdio.h>
//nに入れた数だけ素数を返す
void prime_n(int n)
{
    int p[100],i,j,flag=0,count=0,k;
    for(i=2;count<n;i++){
        for(j=2;j<i;j++){
            if(i%j==0) {
                flag=1;
                break;
            }
        }
        if(flag==0)p[count++]=i;
        flag=0;
    }
    for(k=0;k<count;k++){
        printf("%d\n",p[k]);
    }
}
int main(void)
{
    int n;
    do{
    printf("Input num:");scanf("%d",&n);
    }while(n>100||n<0);
    prime_n(n);
    return 0;
}
