#include<stdio.h>
// 素因数の積を返す関数
int rad (int k) {
    int i, n;
    int flag; // nが素数かどうか判定するためのflag
    int count; // nがkの素因数として現れる回数
    int result_mult = 1; // 素因数の積を入れる変数
    
    // 2以上k以下の整数nが素数かどうか判定
    for(n = 2; n <= k; n++) {
        flag = 1; // 各nに対し、まずflagを1に設定（0になれば素数でないと判定）
        for(i = 3; i <= n; i++) {
            if (n % i == 0) {  // nが割り切れたら
                flag = 0;  // 素数ではない
                break;
            }
        }
        if(flag == 1) { // nが素数のとき
            count = 0;
            while(k%n == 0) { // kがnで割り切れるとき
                k = k/n;
                count++; // nで何回割るか（つまりkの素因数としてnが現れる数）を保持
            }
            if(count != 0) { // nが元のkの素因数であるとき、
                result_mult = result_mult * n; // 素因数nを掛ける
            }
        }
        if(k == 1) break; // 計算過程で、新たなkの値が1となればfor文終了
    }
    return result_mult;
}

int main(void)
{
    int k;
    printf("Input k:");
    scanf("%d",&k);
    printf("%d\n",rad(k));
    return 0;
}

