module CodeGen where

import AST

preambule :: String
preambule = "#include <stdio.h>\n\ntypedef struct сommand{\nint t;\nint arg1;\nint arg2;\n} command;\n\n"

post :: String
post = "void cycle(){ \n    int i = 0; \n    while(smetana[i].t != 0){ \n        switch(smetana[i].t){ \n            case 0: \n            break; \n            case 1: \n            i = smetana[i].arg1; \n            break; \n            case 2: \n            i += 1; \n            command temp = smetana[smetana[i].arg2]; \n            smetana[smetana[i].arg2] = smetana[smetana[i].arg1]; \n            smetana[smetana[i].arg1] = temp; \n            break; \n        } \n    } \n} \nvoid printer(){ \n    for (int i = 0; i < n - 1; i++){ \n        switch(smetana[i].t){ \n            case 0: \n            printf(\"%s\", \"WTF\"); \n            break; \n            case 1: \n            printf(\"%s%u%s%u%s\", \"Step \", i,\". Go to step \", smetana[i].arg1,\".\n\"); \n            break; \n            case 2: \n            printf(\"%s%u%s%u%s%u%s\", \"Step \", i, \". Swap step \", smetana[i].arg1, \" with step \", smetana[i].arg2, \".\"); \n            break; \n        } \n    } \n} \nint main(){ \n    cycle(); \n    printer(); \n    return 0; \n}"

smetanaToList :: Smetana -> [Step]
smetanaToList (Smetana a b) = a : smetanaToList b
smetanaToList End = []

commandToCPP :: Command -> String
commandToCPP (GoTo a) = concat ["{1, ", show a, ", 0}"]
commandToCPP (Swap a b) = concat ["{2, ", show a ,", ", show b, "}"]

smetanaToCPP :: Smetana -> String
smetanaToCPP smet = concat [preambule, "const int n = ", show . length . smetanaToList $ smet,";\n\ncommand smetana[n] = {", recur smet, "};\n\n", post] where
    recur :: Smetana -> String
    recur (Smetana (Step n v) xs) = concat [commandToCPP v, ", ", recur xs]
    recur End = "{0, 0, 0}"

{-
#include <stdio.h>

typedef struct сommand{
    int t;
    int s1;
    int s2;
} command;

const int n = 4;

сommand a[n] = {{1, 2, 0}, {1, 3, 6}, {2, 3, 3}, {0, 0, 0}};

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
            printf("%s%u%s%u%s%u%s", "Step ", i, ". Swap step ", smetana[i].arg1, " with step ", smetana[i].arg2, ".");
            break;
        }
    }
}

int main(){
    cycle();
    printer();
    return 0;
}
-}