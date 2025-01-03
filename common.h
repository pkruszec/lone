#ifndef COMMON_H
#define COMMON_H

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define ARRAY_COUNT(a) (sizeof(a) / sizeof(a[0]))

typedef struct {
    const char *path;
    int line;
    int col;
} Location;

void msg(const char *compiler_file, int compiler_line, const char *mode, Location loc, const char *fmt, ...);

#define compiler_fatal() exit(1)
#define info(...) msg(__FILE__, __LINE__, "info", __VA_ARGS__)
#define note(...) msg(__FILE__, __LINE__, "note", __VA_ARGS__)
#define error(...) msg(__FILE__, __LINE__, "error", __VA_ARGS__)
#define warn(...) msg(__FILE__, __LINE__, "warning", __VA_ARGS__)
#define error_exit(...) do {error(__VA_ARGS__); compiler_fatal();} while (0)

void stretch_buffer(int sz, void **data, int *allocated, int *count, int add);

#define append_many(a, c)      (stretch_buffer(sizeof(*(a)->data), (void **)&(a)->data, &(a)->allocated, &(a)->count, (c)), &(a)->data[(a)->count - c])
#define append_many_zero(a, c) (memset(append_many(a, c), 0, sizeof(*(a)->data) * (c)))
#define append(a)              (append_many(a, 1))
#define append_zero(a)         (append_many_zero(a, 1))
#define append_value(a, v)     (append(a), (a)->data[(a)->count - 1] = (v), (a)->count - 1)

typedef struct {
    int *data;
    int allocated;
    int count;
} Ints;

typedef struct {
    char *data;
    int count;
} String_View;

typedef struct {
    char *data;
    int allocated;
    int count;
} String_Builder;

#define sv_unwrap(sv) (sv).count, (sv).data

String_View sv_make(char *data, int count);
bool  sv_equal(String_View lhs, String_View rhs);
int   sv_has(String_View sv, char c);
char *sb_append(String_Builder *sb, char c);

#endif // COMMON_H
