#include "cons.h"
#include "util.h"
#include "primops.h"

std::string to_s(enum type_t type)
{
  switch ( type ) {
  default:       return "<?>";      break;
  case NIL:      return "nil";      break;
  case INTEGER:  return "integer";  break;
  case CLOSURE:  return "cloure";   break;
  case PAIR:     return "pair";     break;
  case SYMBOL:   return "symbol";   break;
  case STRING:   return "string";   break;
  case U8VECTOR: return "U8VECTOR"; break;
  case CONTINUATION: return "continuation"; break;
  }
}

std::string to_s(cons_t *p)
{
  switch ( type_of(p) ) {
  default:       return "<?>";
  case NIL:      return "<nil>";
  case INTEGER:  return to_s(p->integer);
  case CLOSURE:  return format("<closure %p>", p->closure);
  case PAIR:     return to_s(car(p)) + " . " + to_s(cdr(p));
  case SYMBOL:   return p->symbol->name;
  case STRING:   return p->string;
  case U8VECTOR: return format("<u8vector %p>", p->u8vector);
  case CONTINUATION: return format("<continuation %p>", p->continuation);
  }
}

