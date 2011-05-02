#include "print.h"
#include "util.h"

std::string sprint(cons_t* p, std::string& s, bool escape)
{
  switch ( type_of(p) ) {
  case NIL:          return s;
  case BOOLEAN:      return s + to_s(p->boolean);
  case CHAR:         return s + (escape? to_s(p->character) : "" + p->character);
  case DECIMAL:      return s + to_s(p->decimal);
  case INTEGER:      return s + to_s(p->integer);
  case CLOSURE:      return s + (escape? to_s(p->closure) : "");
  case SYMBOL:       return s + p->symbol->name();
  case STRING:       return s + (escape? "\"" + encode_str(p->string) + "\"" : p->string);
  case VECTOR:       return s + (escape? to_s(p->vector) : "");
  case CONTINUATION: return s + (escape? to_s(p->continuation) : "");
  case PAIR: {
    std::string head = sprint(car(p), s, escape);
    std::string tail = (atomp(cdr(p)) && !nullp(cdr(p)) ?
                        ". " : "") + sprint(cdr(p), s, escape);
    return s
      + (listp(car(p)) ? "(" : "")
      + head
      + (listp(car(p)) ? ")" : "")
      + (!tail.empty() ? " " : "") + tail;
  }}

  return s;
}

std::string sprint(cons_t* p)
{
  std::string s;
  return sprint(listp(p) ? cons(p) : p, s, true);
}

std::string sprint(program_t* p)
{
  return sprint(p->root);
}

std::string print(cons_t* p)
{
  std::string s;
  return sprint(list(p) ? cons(p) : p, s, false);
}

std::string print(program_t* p)
{
  return print(p->root);
}
