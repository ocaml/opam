#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#if defined(unix) || defined(__unix__) || defined(__unix)
#include <sys/utsname.h>

value builtin_uname(value unit)
{
  CAMLparam1(unit);
  CAMLlocal2 (r, st);
  struct utsname un;

  uname(&un);

  st = caml_alloc(3, 0);
  Store_field(st, 0, caml_copy_string(un.sysname));
  Store_field(st, 1, caml_copy_string(un.release));
  Store_field(st, 2, caml_copy_string(un.machine));

  r = caml_alloc(1, 0);
  Store_field(r, 0, st);

  CAMLreturn (r);
}
#else
value builtin_uname(value unit)
{
  CAMLparam1(unit);
  CAMLreturn (Val_none);
}
#endif
