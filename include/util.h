#ifndef INC_MICKEY_UTIL_H
#define INC_MICKEY_UTIL_H

#include <string>

std::string to_s(int n);
std::string format(const char *fmt, ...);
std::string toupper(const char* s);

char* copy_str(const char* s);
char* trimr(char* s);
const char* skip_space(const char* s);

int empty(const char*);
bool char_in(char ch, const char* s);

char* decode_literal_string(const char* s);

#endif
