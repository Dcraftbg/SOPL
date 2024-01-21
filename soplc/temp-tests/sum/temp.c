#include <stdio.h>

int sum(int a, int b);
int sumTwo(int a);
int main(void) {
    {
    int a = 5;
    printf("sumTwo(%d) => %d\n", a, sumTwo(a));
    }
    {
    int a = 5;
    int b = 4;
    printf("sum(%d,%d) => %d\n", a, b, sum(a,b));
    }
}