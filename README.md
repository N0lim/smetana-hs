# smetana-hs
SMETANA -> C compiler written on Haskell
# Linux
cabal build && cabal run
yourFile.smetana -flag1 -flag2

# All flags
-toC - generates C file
-toO - generates C and out file through gcc compiler
