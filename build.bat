@echo off

if not exist build mkdir build
pushd build
clang -o lone.exe -Wall -Wextra -std=c99 -g ..\main.c
popd
