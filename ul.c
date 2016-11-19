/* lisp interpreter  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define KEEP 0
#define TERMINATE 1
#define SKIP 2
char rt[0x80];
int look; /* look ahead character */
char value[16]; /* token */
void * heap_base;
void * heap_ptr;
void * sym_ptr;
#define num_tag  0x0 /* 0000 */
#define pair_tag 0x1 /* 0001 */
#define sym_tag  0x3 /* 0011 */
#define proc_tag 0x7 /* 0111 */
#define is_pair(x)  (((long)x & 7) == pair_tag)
#define is_sym(x)   (((long)x & 7) == sym_tag)
#define is_num(x)   (((long)x & 7) == num_tag)
#define is_proc(x)  (((long)x & 7) == proc_tag)
#define set_pair(x) ((long)x | pair_tag)
#define set_sym(x)  (((long)x << 3) | sym_tag)
#define set_num(x)  ((long)x << 3)
#define set_proc(x) (((long)x << 3) | proc_tag)
#define car(x)      (void *) *(long*) ((long)x - 1)
#define cdr(x)      (void *) *(long*) ((long)x + 7)
void read_table_default() {
  int c;
  for(c = 0 ; c < 0x7f; c++ ) rt[c] = KEEP;
  rt['(']=rt[')']=rt['.']=rt[';']=rt['\'']=rt['"']=rt['#']=rt[',']=rt['`']=
    TERMINATE;
  rt[' ']=rt['\t']=rt['\r']=rt['\n']= SKIP;
}
void lookahead() { look = getchar(); }
void gettoken() {
  int value_index=0;
  while(rt[look] == SKIP)
    lookahead();
  if (rt[look] == TERMINATE) {
    value[0] = look;
    lookahead();
    return;
  }
  while(rt[look] == KEEP) {
    value[value_index++] = look;
    lookahead();
    if (look == EOF) break;
  }
  value[value_index] = '\0';
}

void * cons(void *, void *);
void * intern(char *);
void * getlist();
void * getobj(int);
void print_obj(void *ob, int head_of_list);

void *toplevel, *g_define, *g_cond, *g_nil, *g_begin, *g_true,
  *g_atom, *g_quote, *g_eq, *g_car, *g_cdr, *g_cons, *g_cond, *g_label, *g_lambda ;

void * findsym(char *val, void *list) {
  for (;list; list = cdr(list)) {
    void *a = car(list);
    if (is_sym(a)) {
      if (strcmp( (char *) ((long) a >> 3), val) == 0)
        return a;
    }
  }
  return 0;
}

void * assoc(void *v, void *e) {
  for ( ; e; e = cdr(e)) {
    void *a = car(e);
    if (v == car(a)) return car(e);
  }
  return g_nil;
}

void * intern(char *val) {
  void *base = heap_ptr;
  char *p;
  p = findsym(val, sym_ptr);
  if (p) return p;
  p = heap_ptr;
  while((*p++ = *val++))
    heap_ptr = (void*)  ((long)heap_ptr + 1);
  heap_ptr = (void*) ((long) heap_ptr + (sizeof(void *) * 2) - (long) heap_ptr % (sizeof(void *) * 2));
  base = (void *)set_sym(base);
  sym_ptr = cons(base, sym_ptr);
  return base;
}

void * cons(void *a, void *b) {
  void *cell = heap_ptr;
  *(void **)cell = a;
  *((void **) ((long)cell + sizeof(void*))) = b;
  heap_ptr = (void*) ((long)heap_ptr + sizeof (void *) * 2);
  return (void *) set_pair(cell);
}

void * proc(void * (*fn ) (void *) ) {
  void *p = heap_ptr;
  *(void **)p = * (void **) &fn;
  heap_ptr = (void*) ((long)heap_ptr + sizeof (void *) * 2);
  return (void *) set_proc(p);
}

void * getobj(int have_token) {
  if (have_token == 0) gettoken();
  if (look==EOF) return g_nil;
  if (value[0]=='(') {
    return getlist();
  } else if (value[0]=='\'') {
    return cons(g_quote, cons(getobj(0), 0));
  } else if (value[0]>='0' && value[0] <='9') {
    long *l = (long *) heap_ptr;
    *l = atoi(value);
    *l = set_num(*l);
    heap_ptr = (void*) ((long)heap_ptr + sizeof (void *) * 2);
    return (void *) *l; /* immediate type */
  }
  return intern(value);
}

void * getlist() {
  void *tmp;
  gettoken();
  if (value[0]==')')
    return 0;
  else if (value[0]=='.') {
    tmp = getobj(0);
    gettoken();
    if (value[0]==')') return tmp;
    return tmp;
  }
  tmp = getobj(1); /* token unget */
  return cons(tmp, getlist());
}

void print_obj(void *ob, int head_of_list) {
  if (is_num(ob) ) {
    printf("%ld", ((long) ob) >> 3);
  } else if (is_sym(ob) ) {
    printf("%s", (char *) ((long) ob >> 3));
  } else if (is_pair(ob) ) {
    void *a = car(ob),
         *b = cdr(ob);
    if (head_of_list) printf("(");
    print_obj(a, 1);
    if (b != 0) {
      if (is_pair(b) == 0) {
        printf(" . ");
        print_obj(b, 0);
        printf(")");
      } else if (is_pair(b) == 1) {
        printf(" ");
        print_obj(b, 0);
      }
    } else {
      printf(")");
    }
  } else if (is_proc(ob)) {
    printf("<procedure %p>", ob);
  } else if (ob == g_nil) {
    printf("()");
  } else {
    printf("<%p>", ob);
  }
}

#define debug(OBJ) \
  printf("%s:%d ", __FILE__, __LINE__); \
  print_obj(OBJ,1); \
  printf("\n"); \

void *evlist(void *, void*);

void * eval(void *ob, void *en) {
  if (is_num(ob)) {
    return ob;
  } else if (is_sym(ob)) {
    void *a = assoc(ob, en);
    if (a == g_nil) return g_nil;
    if (is_pair(a)) return car(cdr(a)); /* (a b c) -> b */
    return g_nil;
  } else if (is_pair(ob)) {
    void *a = car(ob), *b = cdr(ob);
    if (a == g_nil) {
      return g_nil;
    } else if (a == g_label || a == g_define) {
      toplevel = cons(b, toplevel);
    } else if (a == g_quote) {
      return car(cdr(ob));
    } else if (a == g_atom) {
      void *r = eval(car(cdr(ob)), en);
      return  is_pair(r) == 0 ? intern("t") : g_nil;
    } else if (a == g_eq) {
      void *r1 = eval(car(cdr(ob)), en),
           *r2 = eval(car(cdr(cdr(ob))), en);
      return r1 == r2 ? intern("t") : g_nil;
    } else if (a == g_car) {
      void *r = eval(car(cdr(ob)), en);
      return car(r);
    } else if (a == g_cdr) {
      void *r = eval(car(cdr(ob)), en);
      return cdr(r);
    } else if (a == g_cons) {
      void *r1 = eval(car(cdr(ob)), en),
           *r2 = eval(car(cdr(cdr(ob))), en);
      return cons(r1, r2);
    } else if (a == g_cond) {
      for ( ; b ; ) {
        void *r = eval(car(car(b)), en);
        if (r != g_nil) {
          return eval(car(cdr(car(b))), en);
        }
        b = cdr(b);
      }
    } else if (a == g_lambda) {
      return ob;
    } else if (is_sym(a)) {
      void *r = eval(a,en);
      if (is_proc(r)) {
        void *fn = *(void **) ((long)r >> 3);
        void *al = evlist(b, en);
        return ((void * (*) (void *)) fn) (al);
      } else {
        return eval(cons(r, b), en);
      }
    } else if (is_pair(a)) {
      /* eval arguments, add to environment, eval body */
      /* ((lambda(x y) x) 1 2) */
      if (car(a) == g_lambda) {
        void *x, *y, *z;
        x = car(cdr(a));
        y = b;
        for ( ; x; ) {
          z = cons(car(x), cons(eval(car(y),en), 0));
          x = cdr(x);
          y = cdr(y);
          en = cons(z, en);
        }
        return eval(car(cdr(cdr(a))), en);
      }
    } /* is pair */
  }
  return g_nil;
}

void *evlist(void *l, void *e) {
  if (l==0) return 0;
  return cons(eval(car(l),e), evlist(cdr(l), e));
}

#define make_proc(NAME,OP) \
void *NAME(void *args) { \
  long r = ((long) car(args) >> 3); \
  args = cdr(args); \
  for (;args; args = cdr(args)) \
    r = r OP ((long) car(args) >> 3); \
  return (void *)set_num(r);\
}

make_proc(add,+)
make_proc(sub,-)
make_proc(mul,*)

int main(int argc, char *argv[]) {
  void *ob;
  heap_base=malloc(8192);
  heap_ptr = heap_base;
  sym_ptr = 0;
  g_nil = intern("()");
  g_true = intern("t");
  toplevel = 0;
  toplevel = cons(cons(intern("+"), cons(proc(add), 0)), toplevel);
  toplevel = cons(cons(intern("-"), cons(proc(sub), 0)), toplevel);
  toplevel = cons(cons(intern("*"), cons(proc(mul), 0)), toplevel);
  g_begin = intern("begin");
  g_quote = intern("quote");
  g_atom = intern("atom");
  g_eq = intern("eq");
  g_car = intern("car");
  g_cdr = intern("cdr");
  g_cons = intern("cons");
  g_cond = intern("cond");
  g_label = intern("label");
  g_lambda = intern("lambda");
  g_define = intern("define");
  read_table_default();
  lookahead();
  for (;;) {
    ob = getobj(0);
    if (look == EOF) break;
    ob = eval(ob, toplevel);
    print_obj(ob, 1);
    printf("\n");
  }
  return 0;
}
