#include <stdio.h>

typedef struct сommand{
int t;
int arg1;
int arg2;
} command;

const int n = 7;

command smetana[n] = {{1, 3, 0}, {2, 2, 4}, {1, 5, 0}, {2, 0, 5}, {1, 1, 0}, {2, 0, 1}, {0, 0, 0}};

void cycle(){ 
    int i = 0; 
    while(smetana[i].t != 0){ 
        switch(smetana[i].t){ 
            case 0: 
            break; 
            case 1: 
            i = smetana[i].arg1; 
            break; 
            case 2: 
            i += 1; 
            command temp = smetana[smetana[i].arg2]; 
            smetana[smetana[i].arg2] = smetana[smetana[i].arg1]; 
            smetana[smetana[i].arg1] = temp; 
            break; 
        } 
    } 
} 
void printer(){ 
    for (int i = 0; i < n - 1; i++){ 
        switch(smetana[i].t){ 
            case 0: 
            printf("%s", "WTF"); 
            break; 
            case 1: 
            printf("%s%u%s%u%s", "Step ", i,". Go to step ", smetana[i].arg1,".\n"); 
            break; 
            case 2: 
            printf("%s%u%s%u%s%u%s", "Step ", i, ". Swap step ", smetana[i].arg1, " with step ", smetana[i].arg2, ".\n"); 
            break; 
        } 
    } 
} 
int main(){ 
    cycle(); 
    printer(); 
    return 0; 
}