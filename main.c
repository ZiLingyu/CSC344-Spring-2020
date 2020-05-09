#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 256

void strip(char *buffer) {
    int index = strlen(buffer) -1;
    while(index >= 0 && buffer[index] == '\n') {
        buffer[index] = '\0';
        index--;
    }
}

int main() {
    FILE *fptr;
    int counter = 0;
    int numOfLine = 0;
    char line[BUFFER_SIZE];
    char** dataBuffer;
    int index;

    if ((fptr = fopen("/Users/lingyuzi/Desktop/Spring 2020/CSC344/C/Assignment/MicroProject/Text.txt", "r")) == NULL) {
        printf("File does not exist.\n");
        // Program exits if the file pointer returns NULL
        exit(1);
    }

    while(fgets(line, BUFFER_SIZE, fptr) != NULL) {
        strip(line);
        if (counter == 0) {
            sscanf(line, "%d", &numOfLine);
            dataBuffer = malloc(sizeof(char *) * numOfLine);
        } else {
            dataBuffer[counter - 1] = malloc(strlen(line) + 1);
            strncpy(dataBuffer[counter - 1], line, BUFFER_SIZE);
        }
        counter++;
    }

    while(1) {
        printf("Please enter the index from 0 to %d: ", numOfLine - 1);
        scanf("%d", &index);
        if (index < 0) {
            break;
        }
        if (index < numOfLine) {
            printf("%s\n", dataBuffer[index]);
        }
    }

    return 0;
}
