#include <stdio.h>
int addTwo(int a)  {
    return a+2;
}
int addThree(int a);
int main(void) {
    int a = 5;
    printf("addThree(%d) => %d", a, addThree(a));
}