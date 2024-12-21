set -xe

cc -ggdb -Wall -Wextra -o lone main.c
./lone expr.ln
