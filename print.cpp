#include "print.h"
#include "util.h"

std::string sprint(cons_t* p, std::string& s, bool escape)
{
  if ( p != NULL )
  switch ( p->type ) {
  case NIL: return "";
  case BOOLEAN: return s + to_s(p->boolean);
  case INTEGER: return s + to_s(p->integer);
  case CLOSURE: return s + (escape? "<closure>" : "");
  case PAIR: {
    bool parens = listp(car(p));
    std::string sprint_car = sprint(p->car, s, escape);
    return s
      + (parens? "(" : "")
      + sprint_car
      + (parens? ")" : "")
      + (atomp(cdr(p)) && !nullp(cdr(p)) ? " ." : "")
      + ( (atomp(car(p)) && !nullp(cdr(p)) && !listp(cdr(p))) || // <= THIS
          (!nullp(cdr(p)) && !listp(cadr(p))) ||                 // <=  IS
          (atomp(p) && listp(cdr(p))) ||                         // <= VERY
          (integerp(car(p)) && listp(cdr(p)))                    // <= MESSY! (and wrong)
            ? (escape && !sprint_car.empty()? " " : "") : "")
      + sprint(p->cdr, s, escape);
    } break;
  case SYMBOL: return s + p->symbol->name;
  case STRING: return s + (escape? "\"" + encode_str(p->string) + "\"" : p->string);
  case U8VECTOR: return s + (escape? "<u8vector>" : "");
  case CONTINUATION: return s + (escape? "<continuation>" : "");
  }

  return s;
}

std::string sprint(cons_t* p)
{
  std::string s;
  return sprint(p, s, true);
}

std::string sprint(program_t* p)
{
  return sprint(p->root);
}

std::string print(cons_t* p)
{
  std::string s;
  return sprint(p, s, false);
}

std::string print(program_t* p)
{
  return print(p->root);
}
