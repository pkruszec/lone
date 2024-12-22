@echo off

if not exist build mkdir build
pushd build
clang -o lone.exe -D_CRT_SECURE_NO_WARNINGS -Wall -Wextra -std=c99 -g ..\main.c
popd
