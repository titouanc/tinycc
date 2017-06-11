#include <stdio.h>

extern int tiny();

int print(char c){
    return printf("%c", c);
}

int read(){
    return getchar();
}

int main(int argc, const char **argv)
{
    return tiny();
}