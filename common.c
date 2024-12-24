#include "common.h"

void msg(const char *compiler_file, int compiler_line, const char *mode, Location loc, const char *fmt, ...)
{
    if (0) {
        fprintf(stderr, "%s:%d: ", compiler_file, compiler_line);
    }
    
    if (loc.path)
        fprintf(stderr, "%s:%d:%d: ", loc.path, loc.line + 1, loc.col + 1);
    else
        fprintf(stderr, "%d:%d: ", loc.line + 1, loc.col + 1);

    fprintf(stderr, "%s: ", mode);
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    va_end(args);
}

void stretch_buffer(int sz, void **data, int *allocated, int *count, int add)
{
    if (*count + add > *allocated) {
        int min = *count + add - *allocated;
        *allocated += MAX(min, *allocated);
        *data = realloc(*data, *allocated * sz);
    }
    *count += add;
}

String_View sv_make(char *data, int count)
{
    return (String_View) {
        .data = data,
        .count = count,
    };
}

bool sv_equal(String_View lhs, String_View rhs)
{
    if (lhs.count != rhs.count) return false;
    for (int i = 0; i < lhs.count; ++i) {
        if (lhs.data[i] != rhs.data[i])
            return false;
    }
    return true;
}

int sv_has(String_View sv, char c)
{
    for (int i = 0; i < sv.count; ++i) {
        if (sv.data[i] == c) return i;
    }
    return -1;
}

char *sb_append(String_Builder *sb, char c)
{
    char *p = append_many(sb, 1);
    *p = c;
    return p;
}
