module CodeGen where

import AST

preambule :: String
preambule = "#include <stdio.h>\n\ntypedef struct сommand{\nint t;\nint arg1;\nint arg2;\n} command;\n\n"

post :: String
post = ""

smetanaToList :: Smetana -> [Step]
smetanaToList (Smetana a b) = a : smetanaToList b
smetanaToList End = []

commandToLLVM :: Command -> String
commandToLLVM (GoTo a) = concat ["{1, ", show a, ", 0}"]
commandToLLVM (Swap a b) = concat ["{2, ", show a ,", ", show b, "}"]

smetanaToLLVM :: Smetana -> String
smetanaToLLVM smet = concat ["const int n = ", show . length . smetanaToList $ smet,";\n\ncommand smetana[n] = {", recur smet, "};\n\n", post] where
    recur :: Smetana -> String
    recur (Smetana (Step n v) xs) = concat [commandToLLVM v, ", ", smetanaToLLVM xs]
    recur End = "{0, 0, 0}"

{-
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