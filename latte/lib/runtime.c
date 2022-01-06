#include <stdio.h>
#include <stdlib.h>

void printInt(int x) {
    printf("%d\n", x);
}

void printString(char* str) {
    printf("%s\n", str);
}

void error() {
    printf("runtime error\n");
}

int readInt() {
    int result;
    scanf("%d\n", &result);
    return result;
}

char* readString() {
    char *line = NULL;
    size_t length = 0;
    ssize_t characters_num = 0;

    characters_num = getline(&line, &length, stdin);

    line[characters_num - 1] = '\0';
    return line;
}
