#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct StackNode {
    struct StackNode * next;
    int num;
} StackNode;

typedef struct Node {
    struct Node * next;
    char * op;
    int opData;
} Node;

void addToList(Node ** head, Node* toAdd) {
    if (*head == NULL) {
        *head = toAdd;
        return;
    }
    Node * current = *head;
    while (current->next != NULL) {
        current = current->next;
    }
    current->next = toAdd;
}

void push(StackNode** stack, StackNode * toPush) {
    if (*stack == NULL) {
        *stack = toPush;
        return;
    }
    StackNode* oldStack = *stack;
    *stack = toPush;
    (*stack)->next = oldStack;
}

void pushToEnd(StackNode** stack, StackNode * toPush) {
    if (*stack == NULL) {
        *stack = toPush;
        return;
    }
    StackNode * current = *stack;
    while (current->next != NULL) {
        current = current->next;
    }
    current->next = toPush;
}

StackNode* pop(StackNode** stack) {
    if (*stack == NULL) {
        return NULL;
    }
    StackNode* toPop = *stack;
    *stack = (*stack)->next;
    return toPop;
}

void duplicateTopNElements(StackNode ** stack, int n) {
    // Create a new stack with the same n elements
    StackNode * newStack = NULL;
    StackNode* current = *stack;
    // Iterate over the n elements
    unsigned int counter = 0;

    while(current != NULL && counter < n) {
        // Create a new node for each of the n elements
        StackNode * newNode = (StackNode*) malloc(sizeof(StackNode));
        newNode->num = current->num;
        newNode->next = NULL;
        // Add that new node to the new stack
        pushToEnd(&newStack, newNode);
        counter++;
        current = current->next;
    }
    // Add the new stack to the old stack.
    // Go to the end of the new stack
    StackNode * endPtr = newStack;
    while(endPtr->next != NULL) {
        endPtr = endPtr->next;
    }
    // Set the next pointer of the end of the new stack to be the head of the old stack
    endPtr->next = *stack;
    // Head of old stack is now the head of the new stack
    *stack = newStack;
}

void swap (StackNode ** stack) {
    StackNode * first = pop(stack);
    first->next = NULL;
    StackNode * second = pop(stack);
    second->next = NULL;
    push(stack, first);
    push(stack, second);
}

// Set current to point to the n-th element in the linked list contained by head
void jump(Node ** current, Node * head, int n)
{
    *current = head;
    for (int i = 1; i < n; i++)
    {
        *current = (*current)->next;
    }
}

void printAll(struct Node* head) {
    struct Node* current = head;
    while(current != NULL)
    {
        printf("%s",current->op);
        printf("%d\n",current->opData);
        current = current->next;
    }
}

void printStack(StackNode * stack)
{
    StackNode* current = stack;
    printf("[");
    int i = 0;
    while(current != NULL && i < 20)
    {
        printf("%d ",current->num);
        current = current->next;
        i++;
    }
    printf("]\n");
}

int main() {
    char filename[256];
    printf("Please enter the stack machine instructions filename: ");
    scanf("%s", filename);

    FILE *file = fopen(filename, "r");
    while (file == NULL) {
        printf("Please enter a valid stack machine instructions filename: ");
        scanf("%s", filename);
        file = fopen(filename, "r");
    }

    Node *head = NULL;
    char *line = (char*) malloc(256*sizeof(char));
    size_t length;

    getline(&line, &length, file);

    unsigned int numLines = atoi(line);
    unsigned int counter = 0;

    while (getline(&line, &length, file) != -1 && counter < numLines) {
        // Get first operation
        char *tok = strtok(line, " ");
        Node *newNode = (struct Node *) malloc(sizeof(struct Node));
        char *operation = strdup(tok);

        char *newLine = strchr(operation, '\n');
        if (newLine != NULL) {
            *newLine = '\0';
        }

        newNode->op = operation;
        if (strcmp(tok, "LIT") == 0 || strcmp(tok, "DUP") == 0 || strcmp(tok, "IFEQ") == 0 ||
            strcmp(tok, "IFLT") == 0 || strcmp(tok, "JUMP") == 0) {
            // Get second operation
            tok = strtok(NULL, " ");
            int operationData = atoi(tok);
            newNode->opData = operationData;
        }
        addToList(&head, newNode);
        counter++;
    }
    // printAll(head);

    Node *currentOp = head;
    StackNode *stack = NULL;
    int jumped = 0;
    while (currentOp != NULL) {
        if (strcmp(currentOp->op, "IN") == 0) {
            // IN -- Reads the next integer from the input stream, and pushes it on the stack
            int input = 0;
            scanf("%d", &input);
            StackNode *newNode = (StackNode *) malloc(sizeof(StackNode));
            newNode->num = input;
            newNode->next = NULL;
            push(&stack, newNode);
        } else if (strcmp(currentOp->op, "OUT") == 0) {
            // OUT -- Pops the top integer from the stack, and prints it
            StackNode * popped = pop(&stack);
            printf("%d\n", popped->num);
            free(popped);
        } else if (strcmp(currentOp->op, "LIT") == 0) {
            // LIT [NUMBER] -- Pushes NUMBER on the stack
            int input = currentOp->opData;
            StackNode *newNode = (StackNode *) malloc(sizeof(StackNode));
            newNode->num = input;
            newNode->next = NULL;
            push(&stack, newNode);
        } else if (strcmp(currentOp->op, "DROP") == 0) {
            // DROP -- Pops the top element of the stack
            StackNode *popped = pop(&stack);
            free(popped);
        } else if (strcmp(currentOp->op, "DUP") == 0) {
            // DUP [N] -- Duplicates the top N elements of the stack
            int numToDupe = currentOp->opData;
            duplicateTopNElements(&stack, numToDupe);
        } else if (strcmp(currentOp->op, "SWAP") == 0) {
            // SWAP -- Swaps the top two elements of the stack
            swap(&stack);
        } else if (strcmp(currentOp->op, "ADD") == 0 || strcmp(currentOp->op, "SUB") == 0 ||
                   strcmp(currentOp->op, "MUL") == 0 || strcmp(currentOp->op, "DIV") == 0 ||
                   strcmp(currentOp->op, "MOD") == 0) {
            // ADD/SUB/MUL/DIV/MOD -- Pops the top two integers from the stack, ADDs/.../MODs them, and pushes the result
            StackNode * first = pop(&stack);
            StackNode * second = pop(&stack);
            StackNode * newNode = (StackNode*) malloc(sizeof(StackNode));
            if (strcmp(currentOp->op, "ADD") == 0) {
                newNode->num = first->num + second->num;
            } else if (strcmp(currentOp->op, "SUB") == 0) {
                newNode->num = first->num - second->num;
            } else if (strcmp(currentOp->op, "MUL") == 0) {
                newNode->num = first->num * second->num;
            } else if (strcmp(currentOp->op, "DIV") == 0) {
                newNode->num = first->num / second->num;
            } else if (strcmp(currentOp->op, "MOD") == 0) {
                newNode->num = first->num % second->num;
            }
            push(&stack, newNode);
            free(first);
            free(second);
        } else if (strcmp(currentOp->op, "AND") == 0 || strcmp(currentOp->op, "OR") == 0) {
            // AND/OR -- Pops the top two integers from the stack, ANDs/ORs them, and pushes the result
            StackNode * first = pop(&stack);
            StackNode * second = pop(&stack);
            StackNode * newNode = (StackNode*) malloc(sizeof(StackNode));
            if (strcmp(currentOp->op, "AND") == 0) {
                newNode->num = (first->num) && (second->num);
            } else if (strcmp(currentOp->op, "OR") == 0) {
                newNode->num = (first->num) || (second->num);
            }
            push(&stack, newNode);
            free(first);
            free(second);
        } else if (strcmp(currentOp->op, "IFEQ") == 0) {
            // IFEQ [INSTRUCTION] Checks if the two elements at the top of the stack are equal.
            // If so, jump to the given instruction
            StackNode * first = pop(&stack);
            StackNode * second = pop(&stack);
            if (first->num == second->num) {
                jump(&currentOp, head, currentOp->opData);
                jumped = 1;
            }
            free(first);
            free(second);
        } else if (strcmp(currentOp->op, "IFLT") == 0) {
            // IFLT [INSTRUCTION] -- Checks if the first element of the stack is less than the second element.
            // If so, jump to the given instruction.
            StackNode * first = pop(&stack);
            StackNode * second = pop(&stack);
            if (first->num < second->num) {
                jump(&currentOp, head, currentOp->opData);
                jumped = 1;
            }
            free(first);
            free(second);
        } else if (strcmp(currentOp->op, "JUMP") == 0) {
            // JUMP [INSTRUCTION] -- Jumps to the given instruction.
            jump(&currentOp, head, currentOp->opData);
            jumped = 1;
        }
        if (jumped == 0) {
            currentOp = currentOp->next;
        } else {
            jumped = 0;
        }
        // printStack(stack);
    }
}




