#include "print.h"

std::string sprint(cons_t* p, std::string& s)
{
  if ( p != NULL )
  switch ( p->type ) {
  default: case NIL: return "";
  case INTEGER: return s + to_s(p->integer);
  case CLOSURE: return s + "<closure>";
  case PAIR: {
    bool parens = pairp(car(p));
    return s
      + (parens? "(" : "")
      + sprint(p->car, s)
      + (parens? ")" : "")
      + (atomp(cdr(p)) ? " ." : "")
      + ( (atomp(car(p)) && !nullp(cdr(p)) && !pairp(cdr(p))) || // <= THIS
          (!nullp(cdr(p)) && !pairp(cadr(p))) ||                 // <=  IS
          (atomp(p) && pairp(cdr(p))) ||                         // <= VERY
          (integerp(car(p)) && pairp(cdr(p)))                    // <= MESSY! (and wrong)
            ? " " : "")
      + sprint(p->cdr, s);
    } break;
  case SYMBOL: return s + p->symbol->name;
  case STRING: return s + "\"" + p->string + "\"";
  case U8VECTOR: return s + "<u8vector>";
  case CONTINUATION: return s + "<continuation>";
  }

  return s;
}

std::string sprint(cons_t* p)
{
  std::string s;
  return sprint(p, s);
}

