#include "print.h"
#include "util.h"

std::string sprint(cons_t* p, std::string& s, bool escape)
{
  if ( p != NULL )
  switch ( p->type ) {
  default: case NIL: return "";
  case INTEGER: return s + to_s(p->integer);
  case CLOSURE: return s + (escape? "<closure>" : "");
  case PAIR: {
    bool parens = pairp(car(p));
    return s
      + (parens? "(" : "")
      + sprint(p->car, s, escape)
      + (parens? ")" : "")
      + (atomp(cdr(p)) ? " ." : "")
      + ( (atomp(car(p)) && !nullp(cdr(p)) && !pairp(cdr(p))) || // <= THIS
          (!nullp(cdr(p)) && !pairp(cadr(p))) ||                 // <=  IS
          (atomp(p) && pairp(cdr(p))) ||                         // <= VERY
          (integerp(car(p)) && pairp(cdr(p)))                    // <= MESSY! (and wrong)
            ? (escape? " " : "") : "")
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
