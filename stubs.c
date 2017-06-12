#include <stdio.h>

extern int tiny();

int print(char c){
    int r = putchar(c);
    fflush(stdout);
    return r;
}

int read(){
    return getchar();
}

int main(int argc, const char **argv)
{
    tiny();
    return 0;
}