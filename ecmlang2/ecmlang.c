// std libs
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
// pow func
#include <math.h>
// editline
#include <editline/readline.h>
#include <editline/history.h>
// parser
#include "mpc.h"
#define BUFFER_SIZE 2048
#define ERR_SIZE 512
#define LASSERT(args, cond, fmt, ...) \
  if (!(cond)) { lval_del(args); return create_lval_err(fmt, ##__VA_ARGS__); }
/*
    Function to print abstract syntax trees
*/
void print_ast(mpc_result_t r){
    mpc_ast_print(r.output);
    mpc_ast_delete(r.output);
}

double max(double a, double b){
    return a > b ? a : b;
}

double min(double a, double b){
    return a > b ? b : a;
}

struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

enum {LVAL_NUM, LVAL_ERR, LVAL_SYM, LVAL_SEXP, LVAL_QEXP, LVAL_FUN, LVAL_STR}; // lval.type
enum {LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM}; // lval.err

char *ltype_name(int t){
    switch(t) {
        case LVAL_FUN: return "Function";
        case LVAL_NUM: return "Number";
        case LVAL_ERR: return "Error";
        case LVAL_SYM: return "Symbol";
        case LVAL_SEXP: return "S-Expression";
        case LVAL_QEXP: return "Q-Expression";
        case LVAL_STR: return "String";
        default: return "Unknown";
    }
    return "Unknown type";
}

typedef lval*(*lbuiltin)(lenv*, lval*);

struct lval{
    int type;
    // its values
    double num;
    char *err;
    char *sym;
    char *str;
    // functions
    lbuiltin builtin;
    lenv *env;
    lval *formals;
    lval *body;
    // Expressions
    int counter;
    struct lval **cell;
};

struct lenv{
    // parent environment
    lenv *par;
    // index counter
    int counter;
    char **syms;
    lval **vals;
};

// Some forward declarations
void lval_print(lval *lvalue);
lval *lval_eval_sexpr(lenv *e, lval *lvalue);
lval *builtin(lval *a, char *func);
void lval_del(lval* v);
lval *lval_copy(lval *v);
lval *create_lval_err(char *fmt, ...);
lval *builtin_op(lenv *e, lval* a, char *op);
lval *lval_call(lenv *e, lval *f, lval *a);

mpc_parser_t* Number;
mpc_parser_t* String;
mpc_parser_t* Symbol;
mpc_parser_t* Comment;
mpc_parser_t* Sexpr;
mpc_parser_t* Qexpr;
mpc_parser_t* Expr;
mpc_parser_t* Ecm;

/*
    Constructor for environment
*/
lenv *create_lenv(){
    lenv *env = malloc(sizeof(lenv));
    env->par = NULL;
    env->counter = 0;
    env->syms = NULL;
    env->vals = NULL;
    return env;
}

/*
    Deconstructor for environment
*/
void lenv_del(lenv *e){
    for(int i=0; i<e->counter; i++){
        lval_del(e->vals[i]);
        free(e->syms[i]);
    }
    free(e->syms);
    free(e->vals);
    free(e);
    return ;
}

/*
    Variable finder
*/
lval *lenv_get(lenv *e, lval *k){
    for(int i=0; i<e->counter; i++){
        if(strcmp(e->syms[i], k->sym) == 0){
            return lval_copy(e->vals[i]);
        }
    }
    if(e->par){
        return lenv_get(e->par, k);
    }
    return create_lval_err("Symbol '%s' is unbound", k->sym);
}

/*
    Variable creator
*/
void lenv_put(lenv *e, lval *k, lval *v){
    for(int i=0; i<e->counter; i++){
        if(strcmp(e->syms[i], k->sym) == 0){
            lval_del(e->vals[i]);
            e->vals[i] = lval_copy(v);
            return;
        }
    }
    e->counter++;
    e->vals= realloc(e->vals, sizeof(lval *) * e->counter);
    e->syms= realloc(e->syms, sizeof(char *) * e->counter);

    e->vals[e->counter - 1] = lval_copy(v);
    e->syms[e->counter - 1] = malloc(strlen(k->sym));
    strcpy(e->syms[e->counter - 1], k->sym);
    return;
}

/*
    Copying function for struct lenv
*/

lenv *lenv_copy(lenv *e){
    lenv *n = malloc(sizeof(lenv));
    n->par = e->par;
    n->counter = e->counter;
    n->syms = malloc(sizeof(char *) * n->counter);
    n->vals = malloc(sizeof(lval *) * n->counter);
    for(int i=0; i<e->counter; i++){
        n->syms[i] = malloc(strlen(e->syms[i]) + 1);
        strcpy(n->syms[i], e->syms[i]);
        n->vals[i] = lval_copy(e->vals[i]);
    }
    return n;
}

/*
    Will be used to define global variables
*/
void lenv_def(lenv *e, lval *k, lval *v){
    while(e->par)
        e = e->par;
    lenv_put(e, k, v);
}

/*
    Constructor for user defined functions
*/

lval *create_lval_lambda(lval *formals, lval *body){
    lval *v = malloc(sizeof(lval));
    v->type = LVAL_FUN;
    v->builtin = NULL;
    v->env = create_lenv();
    v->formals = formals;
    v->body = body;
    return v;
}

/*
    Constructor for lval_builtin
*/
lval *create_lval_fun(lbuiltin func){
    lval *lvalue = malloc(sizeof(lval));
    lvalue->type = LVAL_FUN;
    lvalue->builtin = func;
    return lvalue;
}

/*
    Constructor for num type lvals
*/
lval* create_lval_num(double num){
    lval *lvalue = malloc(sizeof(lval));
    lvalue->type = LVAL_NUM;
    lvalue->num = num;
    return lvalue;
}

lval *create_lval_str(char *str){
    lval *v = malloc(sizeof(lval));
    v->type = LVAL_STR;
    v->str = malloc(strlen(str) + 1);
    strcpy(v->str, str);
    return v;
}

/*
    Constructor for symbol typed lvals
*/
lval *create_lval_sym(char *sym){
    lval *lvalue = malloc(sizeof(lval));
    lvalue->type = LVAL_SYM;
    lvalue->sym = malloc(strlen(sym) + 1);
    strcpy(lvalue->sym, sym);
    return lvalue;
}

/*
    Constructor for empty s-expressions
*/
lval *create_lval_sexpr(){
    lval *lvalue = malloc(sizeof(lval));
    lvalue->type = LVAL_SEXP;
    lvalue->counter = 0;
    lvalue->cell = NULL;
    return lvalue;
}

/*
    Constructor for empty q-expression
*/
lval *create_lval_qexpr(){
    lval *lvalue = malloc(sizeof(lval));
    lvalue->type = LVAL_QEXP;
    lvalue->counter = 0;
    lvalue->cell = NULL;
    return lvalue;
}

/*
    Constructor for error typed lvals
*/
lval* create_lval_err(char *fmt, ...){
    lval *lvalue = malloc(sizeof(lval));
    lvalue->type = LVAL_ERR;

    va_list va;
    va_start(va, fmt);

    lvalue->err = malloc(ERR_SIZE);
    vsnprintf(lvalue->err, ERR_SIZE - 1, fmt, va);
    lvalue->err = realloc(lvalue->err, strlen(lvalue->err) + 1);
    va_end(va);
    return lvalue;
}

/*
    Function to read num typed lvals' values
*/
lval *lval_read_num(mpc_ast_t *t){
    double x = 0;
    errno = 0;
    if(t->children_num == 0){
        x = atoi(t->contents);
        return errno != ERANGE ? create_lval_num(x) : create_lval_err("Invalid number");
    }
    x = atoi(t->contents) + ((double)atoi(t->children[1]->contents) / (double)(10 * strlen(t->children[1]->contents)));
    return errno != ERANGE ? create_lval_num(x) : create_lval_err("Invalid number");
}

/*
    Function to remove, delete lvals
*/
void lval_del(lval* v) {
    switch (v->type) {
    /* Do nothing special for number type */
    case LVAL_NUM: break;
    /* Do nothing for function */
    case LVAL_FUN:
        if(!v->builtin){
            lenv_del(v->env);
            lval_del(v->formals);
            lval_del(v->body);
        }
        break;

    /* For Err or Sym free the string data */
    case LVAL_ERR: free(v->err); break;
    case LVAL_SYM: free(v->sym); break;
    case LVAL_STR: free(v->str); break;

    /* If Sexpr or Qexpr then delete all elements inside */
    case LVAL_QEXP:
    case LVAL_SEXP:
        for (int i = 0; i < v->counter; i++) {
        lval_del(v->cell[i]);
        }
        /* Also free the memory allocated to contain the pointers */
        free(v->cell);
        break;
    }
    /* Free the memory allocated for the "lval" struct itself */
    free(v);
    return;
}

/*
    Fills empty s-expression-typed lvals
*/
lval *lval_add(lval *x, lval *y){
    x->counter++;
    x->cell = realloc(x->cell, sizeof(lval *) * x->counter);
    x->cell[x->counter-1] = y;
    return x;
}

lval *lval_add_front(lval *x, lval *y){
    x->counter++;
    x->cell = realloc(x->cell, sizeof(lval *) * x->counter);
    for(int i=x->counter-1; i>0; i--){
        x->cell[i] = x->cell[i-1];
    }
    x->cell[0] = y;
    return x;
}

lval *lval_copy(lval *v){
    lval *x = malloc(sizeof(lval));
    x->type = v->type;
    switch(v->type){
        case LVAL_NUM:
            x->num = v->num;
            break;
        case LVAL_FUN:
            if(v->builtin){
                x->builtin = v->builtin;
            }else{
                x->builtin = NULL;
                x->env = lenv_copy(v->env);
                x->formals = lval_copy(v->formals);
                x->body = lval_copy(v->body);
            }
            break;
        case LVAL_SYM:
            x->sym = malloc(strlen(v->sym) + 1);
            strcpy(x->sym, v->sym);
            break;
        case LVAL_ERR:
            x->err = malloc(strlen(v->err) + 1);
            strcpy(x->err, v->err);
            break;
        case LVAL_STR:
            x->str = malloc(strlen(v->str) + 1);
            strcpy(x->str, v->str);
            break;
        case LVAL_SEXP:
        case LVAL_QEXP:
            x->counter = v->counter;
            x->cell = malloc(sizeof(lval*) * x->counter);
            for(int i=0; i<x->counter; i++)
                x->cell[i] = lval_copy(v->cell[i]);
            break;
    }
    return x;
}

lval *lval_read_str(mpc_ast_t *t){
    /* Cut off the final quote character */
    t->contents[strlen(t->contents)-1] = '\0';
    /* Copy the string missing out the first quote character */
    char* unescaped = malloc(strlen(t->contents+1)+1);
    strcpy(unescaped, t->contents+1);
    /* Pass through the unescape function */
    unescaped = mpcf_unescape(unescaped);
    /* Construct a new lval using the string */
    lval* str = create_lval_str(unescaped);
    /* Free the string and return */
    free(unescaped);
    return str;
}

/*
    Reads the data that has been parsed,
    Creates lvals
*/
lval *lval_read(mpc_ast_t *t){
    lval *x = NULL;
    if(strstr(t->tag, "number"))
        return lval_read_num(t);
    if(strstr(t->tag, "symbol"))
        return create_lval_sym(t->contents);
    if(strstr(t->tag, "string"))
        return lval_read_str(t);

    // it means it is root or sexpr
    if(strcmp(t->tag, ">") == 0)
        x = create_lval_sexpr();
    if(strstr(t->tag, "sexpr"))
        x = create_lval_sexpr();
    if(strstr(t->tag, "qexpr"))
        x = create_lval_qexpr();

    // Fill expressions
    for(int i=0; i<t->children_num; i++){
            if (strstr(t->children[i]->tag, "comment"))
                continue;
            if(strcmp(t->children[i]->contents, "(") == 0)
                continue;
            if(strcmp(t->children[i]->contents, ")") == 0)
                continue;
            if(strcmp(t->children[i]->tag, "regex") == 0)
                continue;
            if(strcmp(t->children[i]->contents, "{") == 0)
                continue;
            if(strcmp(t->children[i]->contents, "}") == 0)
                continue;
            x = lval_add(x, lval_read(t->children[i]));
    }
    return x;
}

/*
    Printing function for expressions,
    e.g. : (+ 5 6) is an expression
*/
void lval_expr_print(lval *x, char open, char close){
    int i=0;
    putchar(open);
    if(x->counter != 0){
        while(i<x->counter-1){
            lval_print(x->cell[i]);
            putchar(' ');
            i++;
        }
        lval_print(x->cell[i]);
    }
    putchar(close);
    return;
}

void lval_print_str(lval *v){
    char *escaped = malloc(strlen(v->str) + 1);
    strcpy(escaped, v->str);
    escaped = mpcf_escape(escaped);
    printf("\"%s\"", escaped);
    free(escaped);
}

/*
    Function that determines how to print lvals
*/
void lval_print(lval *lvalue){
    switch (lvalue->type) {
        case LVAL_NUM:{
            if(lvalue->num - (int)lvalue->num != 0)
                printf("%f", lvalue->num);
            else
                printf("%d", (int)lvalue->num);
            break;
        }
        case LVAL_ERR:   printf("Error: %s", lvalue->err);  break;
        case LVAL_SYM:   printf("%s", lvalue->sym);         break;
        case LVAL_STR:   lval_print_str(lvalue);            break;
        case LVAL_SEXP:  lval_expr_print(lvalue, '(', ')'); break;
        case LVAL_QEXP:  lval_expr_print(lvalue, '{', '}'); break;
        case LVAL_FUN:
            if(lvalue->builtin)
                printf("<builtin>");
            else{
                printf("(lambda ");
                lval_print(lvalue->formals);
                putchar(' ');
                lval_print(lvalue->body);
                putchar(')');
            }
            break;
  }
  return;
}

/*
    Obvious
*/
void lval_println(lval *lvalue){
    lval_print(lvalue); putchar('\n');
}

lval* lval_pop(lval* v, int i) {
    /* Find the item at "i" */
    lval* x = v->cell[i];

    /* Shift memory after the item at "i" over the top */
    for(int j=i; j<v->counter-1; j++)
    v->cell[j] = v->cell[j+1];

    /* Decrease the count of items in the list */
    v->counter--;

    /* Reallocate the memory used */
    v->cell = realloc(v->cell, sizeof(lval*) * v->counter);
    return x;
}

lval *lval_take(lval* v, int i) {
    lval* x = lval_pop(v, i);
    lval_del(v);
    return x;
}

lval* lval_eval(lenv *e, lval* v) {
    if(v->type == LVAL_SYM){
        lval *x = lenv_get(e, v);
        lval_del(v);
        return x;
    }
    /* Evaluate Sexpressions */
    if (v->type == LVAL_SEXP) { return lval_eval_sexpr(e, v); }
    /* All other lval types remain the same */
    return v;
}

lval* lval_eval_sexpr(lenv *e, lval* v) {
    // Evaluate each children, if they are errors return
    for(int i=0; i<v->counter; i++){
        v->cell[i] = lval_eval(e, v->cell[i]);
        if(v->cell[i]->type == LVAL_ERR)
            return lval_take(v, i);
    }

    // If s-expr empty return itself
    if(v->counter == 0)
        return v;

    // if s-expr has only one child, take and return it
    if(v->counter == 1)
        return lval_take(v, 0);

    // Check if s-expr starts with a symbol
    lval *f = lval_pop(v, 0);
    if(f->type != LVAL_FUN){
        lval_del(f);
        lval_del(v);
        return create_lval_err("First element must be a function");
    }

    lval *result = lval_call(e, f, v);
    lval_del(f);
    return result;
}

/*
    Math builtin functions start
*/
lval* builtin_add(lenv* e, lval* a) {
  return builtin_op(e, a, "+");
}

lval* builtin_sub(lenv* e, lval* a) {
  return builtin_op(e, a, "-");
}

lval* builtin_mul(lenv* e, lval* a) {
  return builtin_op(e, a, "*");
}

lval* builtin_div(lenv* e, lval* a) {
  return builtin_op(e, a, "/");
}

lval *builtin_pow(lenv *e, lval *a){
    return builtin_op(e, a, "^");
}

lval *builtin_max(lenv *e, lval *a){
    return builtin_op(e, a, "max");
}

lval *builtin_min(lenv *e, lval *a){
    return builtin_op(e, a, "min");
}
/*
    Math builtin function ends
*/

/*
    List builtin functions start
*/
lval *builtin_head(lenv *e, lval *a){
    //printf("%d %d\n", a->type, LVAL_SEXP);
    LASSERT(a, a->counter == 1, "Passed too many arguments to builtin function 'head'\nGot %d, expected %d",
            a->counter, 1);
    LASSERT(a, a->cell[0]->type == LVAL_QEXP, "Can only perform function 'head' on Q-expressions");
    LASSERT(a, a->cell[0]->counter != 0, "Function 'head' passed {}, empty Q-expression")
    // IF it passes all the tests, take the first argument
    lval *f = lval_take(a, 0);
    while(f->counter > 1){
        lval_del(lval_pop(f, 1));
    }
    return f;
}

lval *builtin_tail(lenv *e, lval *a){
    LASSERT(a, a->counter == 1, "Passed too many arguments to builtin function 'tail'\nGot %d, expected %d",
            a->counter, 1);
    LASSERT(a, a->cell[0]->type == LVAL_QEXP, "Can only perform function 'tail' on Q-expressions");
    LASSERT(a, a->cell[0]->counter != 0, "Function 'tail' passed {}, empty Q-expression")

    lval *f = lval_take(a, 0);
    lval_del(lval_pop(f, 0));
    return f;
}

lval *builtin_list(lenv *e, lval *a){
    a->type = LVAL_QEXP;
    return a;
}

lval *builtin_eval(lenv *e, lval *a){
    LASSERT(a, a->counter == 1, "Function 'eval' passed too many arguments!\nGot %d, expected %d",
            a->counter, 1);
    LASSERT(a, a->cell[0]->type == LVAL_QEXP, "Function 'eval' passed incorrect type!\nGot type %s, expected %s",
            ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXP));

    lval *x = lval_take(a, 0);
    x->type = LVAL_SEXP;
    return lval_eval(e, x);
}

lval *lval_join(lval *x, lval *y){
    while(y->counter){
        lval_add(x, lval_pop(y, 0));
    }
    lval_del(y);
    return x;
}

lval *builtin_join(lenv *e, lval *a){
    for(int i=0; i<a->counter; i++)
        LASSERT(a->cell[i], a->cell[i]->type == LVAL_QEXP, "Function 'join' got the wrong typed data");

    lval *x = lval_pop(a, 0);
    while(a->counter > 0){
        x = lval_join(x, lval_pop(a, 0));
    }
    return x;
}

lval *builtin_cons(lenv *e, lval *a){
    LASSERT(a, a->counter == 2, "Function 'cons' takes two arguments");
    LASSERT(a, a->cell[1]->type == LVAL_QEXP, "Function 'cons' must take a q-expression as second argument");
    lval *f = lval_pop(a, 0);
    lval *x = lval_pop(a, 0);
    x = lval_add_front(x, f);
    return x;
}

lval *builtin_len(lenv *e, lval *a){
    LASSERT(a, a->counter == 1, "Too many arguments passed to function 'len'");
    lval *f = create_lval_num(a->cell[0]->counter);
    lval_del(a);
    return f;
}

lval *builtin_init(lenv *e, lval *a){
    LASSERT(a, a->counter == 1, "Too many arguments passed to function 'init'");
    LASSERT(a, a->cell[0]->type == LVAL_QEXP, "Function 'init' must take q-expression as argument");
    LASSERT(a, a->cell[0]->counter > 0, "Function 'init' passed {}, empty Q-expression");
    lval *f = lval_take(a, 0);
    lval_del(lval_pop(f, f->counter-1));
    return f;
}
/*
    List builtin functions end
*/

/*
    Builtin lambda functions
*/
lval *builtin_lambda(lenv *e, lval *a){
    LASSERT(a, a->counter == 2, "lambda function expected %d arguments, got %d arguments instead",
            2 ,a->counter);
    LASSERT(a, a->cell[0]->type == LVAL_QEXP, "Formal part of the function 'lambda' must be q-expression, it is %s now",
            ltype_name(a->cell[0]->type));
    LASSERT(a, a->cell[1]->type == LVAL_QEXP, "Body part of the function 'lambda' must be q-expression, it is %s now",
            ltype_name(a->cell[1]->type));

    // first q-expr can only contain symbols
    lval *formals = lval_pop(a, 0);
    for(int i=0; i<formals->counter; i++){
        LASSERT(formals->cell[i], formals->cell[i]->type == LVAL_SYM, "Formal can only contain symbols, not %s",
            ltype_name(formals->cell[i]->type));
    }
    lval *body = lval_pop(a, 0);
    lval_del(a);
    return create_lval_lambda(formals, body);
}

lval *builtin_var(lenv *e, lval *a, char *func){
    LASSERT(a, a->cell[0]->type == LVAL_QEXP, "Must be q-expression");

    lval *syms = a->cell[0];
    for(int i=0; i<syms->counter; i++){
        LASSERT(a, syms->cell[i]->type == LVAL_SYM, "Function '%s' cannot define non-symbol arguments",
                func);
    }

    LASSERT(a, syms->counter == a->counter - 1, "Function %s is passed with too many arguments", func);

    for(int i=0; i<syms->counter; i++){
        if(strcmp(func, "def") == 0){
            lenv_def(e, syms->cell[i], a->cell[i+1]);
        }else if(strcmp(func, "=") == 0){
            lenv_put(e, syms->cell[i], a->cell[i+1]);
        }
    }
    return a;
}

/*
    Builtin def function
*/
lval *builtin_def(lenv *e, lval* a){
    return builtin_var(e, a, "def");
}

/*
    Builtin assign function aka put
*/
lval *builtin_put(lenv *e, lval *a){
    return builtin_var(e, a, "=");
}

/*
    Builtin load function
*/
lval *builtin_load(lenv *e, lval *a){
    LASSERT(a, a->counter == 1, "Function 'load' takes exactly 1 argument");
    LASSERT(a, a->cell[0]->type == LVAL_STR, "Function 'load' takes string as argument");

  /* Parse File given by string name */
  mpc_result_t r;
  if (mpc_parse_contents(a->cell[0]->str, Ecm, &r)) {

    /* Read contents */
    lval* expr = lval_read(r.output);
    mpc_ast_delete(r.output);

    /* Evaluate each Expression */
    while (expr->counter) {
      lval* x = lval_eval(e, lval_pop(expr, 0));
      /* If Evaluation leads to error print it */
      if (x->type == LVAL_ERR) { lval_println(x); }
      lval_del(x);
    }

    /* Delete expressions and arguments */
    lval_del(expr);
    lval_del(a);

    /* Return empty list */
    return create_lval_sexpr();

  } else {
    /* Get Parse Error as String */
    char* err_msg = mpc_err_string(r.error);
    mpc_err_delete(r.error);

    /* Create new error message using it */
    lval* err = create_lval_err("Could not load Library %s", err_msg);
    free(err_msg);
    lval_del(a);

    /* Cleanup and return error */
    return err;
  }
}


/*
    Caller function for functions
*/
lval *lval_call(lenv *e, lval *f, lval *a){
    if(f->builtin)
        return f->builtin(e, a);

    while(a->counter){
        if(f->formals->counter == 0){
            lval_del(a);
            return create_lval_err("Function passed too many arguments");
        }
        lval *sym = lval_pop(f->formals, 0);
        if(strcmp(sym->sym, "&") == 0){
            // & must be followed by another symbol
            if(f->formals->counter != 1){
                lval_del(a);
                return create_lval_err("Function format invalid, something must come after '&'");
            }
            lval *next_sym = lval_pop(f->formals, 0);
            lenv_put(f->env, next_sym, builtin_list(e, a));
            lval_del(sym);
            lval_del(next_sym);
            break;
        }
        lval *val = lval_pop(a, 0);
        lenv_put(f->env, sym, val);
        lval_del(sym);
        lval_del(val);
    }
    // Since all the args are bonded
    lval_del(a);

    // If '&' remains in formal list bind to empty list
    if(f->formals->counter > 0 && strcmp(f->formals->cell[0]->sym, "&") == 0){
        // Check to ensure that & is not passed invalidly
        if (f->formals->counter != 2) {
            return create_lval_err("Function format invalid. Symbol '&' not followed by single symbol.");
        }
        lval_del(lval_pop(f->formals, 0));
        lval *sym = lval_pop(f->formals, 0);
        lval *val = create_lval_qexpr();

        // Bind to env and then delete
        lenv_put(f->env, sym, val);
        lval_del(sym);
        lval_del(val);
    }

    if(f->formals->counter == 0){
        f->env->par = e;
        return builtin_eval(f->env, lval_add(create_lval_sexpr(), lval_copy(f->body)));
    }

    return lval_copy(f);
}

/*
    Builtin function to list every environment variable with their values
*/
lval *builtin_show(lenv *e, lval *a){
    LASSERT(a, a->counter == 1, "Too many arguments for function 'show'.\nGot %d, expected %d",
            a->counter, 1);
    //LASSERT(a, a->cell[0]->sym == "def", "Wrong syntax.\nExpected 'show def'");

    for(int i=0; i<e->counter; i++){
        printf("%s ", e->syms[i]);
        lval_println(e->vals[i]);
    }
    return a;
}

lval *builtin_fun(lenv *e, lval *a){
    LASSERT(a, a->counter == 2, "Function 'must' have 2 q-expressions as argument");
    LASSERT(a, a->cell[0]->type == LVAL_QEXP, "Function 'must' have 2 q-expressions as argument\nNot %s",
            ltype_name(a->cell[0]->type));
    LASSERT(a, a->cell[1]->type == LVAL_QEXP, "Function 'must' have 2 q-expressions as argument\nNot %s",
            ltype_name(a->cell[1]->type));


    lval *sym = lval_pop(a->cell[0], 0);
    lval *func = builtin_lambda(e, a);
    lenv_put(e, sym, func);
    lval *fun = lenv_get(e, sym);
    lval_del(func); lval_del(sym);
    return fun;
}

lval *builtin_greater(lenv *e, lval *a){
    LASSERT(a, a->counter == 2, "Function '>' got %d aruguments, should have taken 2", a->counter);
    LASSERT(a, a->cell[0]->type == LVAL_NUM, "First argument of function '>' must be num, not %s",
            ltype_name(a->cell[0]->type));
    LASSERT(a, a->cell[1]->type == LVAL_NUM, "Second argument of function '>' must be num, not %s",
            ltype_name(a->cell[1]->type));

    lval *first = lval_pop(a, 0);
    lval *second = lval_pop(a, 0);

    lval_del(a);

    if(first->num > second->num)
        return create_lval_num(1);

    return create_lval_num(0);
}

lval *builtin_less_or_eq(lenv *e, lval *a){
    LASSERT(a, a->counter == 2, "Function '<=' got %d aruguments, should have taken 2", a->counter);
    LASSERT(a, a->cell[0]->type == LVAL_NUM, "First argument of function '<=' must be num, not %s",
            ltype_name(a->cell[0]->type));
    LASSERT(a, a->cell[1]->type == LVAL_NUM, "Second argument of function '<=' must be num, not %s",
            ltype_name(a->cell[1]->type));

    lval *v = builtin_greater(e, a);
    v->num = !v->num;

    return v;
}

lval *builtin_less(lenv *e, lval *a){
    LASSERT(a, a->counter == 2, "Function '<' got %d aruguments, should have taken 2", a->counter);
    LASSERT(a, a->cell[0]->type == LVAL_NUM, "First argument of function '<' must be num, not %s",
            ltype_name(a->cell[0]->type));
    LASSERT(a, a->cell[1]->type == LVAL_NUM, "Second argument of function '<' must be num, not %s",
            ltype_name(a->cell[1]->type));

    lval *first = lval_pop(a, 0);
    lval *second = lval_pop(a, 0);

    lval_del(a);

    if(first->num < second->num)
        return create_lval_num(1);

    return create_lval_num(0);
}

lval *builtin_greater_or_eq(lenv *e, lval *a){
    LASSERT(a, a->counter == 2, "Function '>=' got %d aruguments, should have taken 2", a->counter);
    LASSERT(a, a->cell[0]->type == LVAL_NUM, "First argument of function '>=' must be num, not %s",
            ltype_name(a->cell[0]->type));
    LASSERT(a, a->cell[1]->type == LVAL_NUM, "Second argument of function '>=' must be num, not %s",
            ltype_name(a->cell[1]->type));

    lval *first = lval_pop(a, 0);
    lval *second = lval_pop(a, 1);

    return create_lval_num(first->num >= second->num);
}

lval *builtin_not(lenv *e, lval *a){
    LASSERT(a, a->counter == 1, "Function 'not' got %d aruguments, should have taken 1", a->counter);
    LASSERT(a, a->cell[0]->type == LVAL_NUM && (a->cell[0]->num == 1 || a->cell[0]->num == 0),
            "Argument must be either 1 or 0 (true, false)");

    return create_lval_num(!a->cell[0]->num);
}

lval *builtin_if(lenv *e, lval *a){
    LASSERT(a, a->counter == 3, "Condition function 'if' can only take 3 argument, not %d", a->counter);
    LASSERT(a, a->cell[0]->type == LVAL_QEXP, "First argument must be q-expr, not %s",
            ltype_name(a->cell[0]->type));
    LASSERT(a, a->cell[1]->type == LVAL_QEXP, "Second argument must be q-expr, not %s",
            ltype_name(a->cell[1]->type));
    LASSERT(a, a->cell[2]->type == LVAL_QEXP, "Third argument must be q-expr, not %s",
            ltype_name(a->cell[2]->type));

    lval *cond = lval_add(create_lval_qexpr(), lval_pop(a, 0));
    lval *body_if_true = lval_add(create_lval_qexpr(), lval_pop(a, 0));
    lval *body_if_false = lval_add(create_lval_qexpr(), lval_pop(a, 0));

    lval_del(a);

    lval *result = builtin_eval(e, cond);
    /*LASSERT(result,result->counter == 0 && result->type == LVAL_NUM && (result->num == 1 || result->num == 0),
            "Invalid input, expected a cond argument (1 or 0), got %s",
            ltype_name(result->type));*/

    if((int)result->num == 0){
        return builtin_eval(e, body_if_false);
    }
    return builtin_eval(e, body_if_true);

}

int lval_eq(lval *first, lval *second){
    if(first->type != second->type)
        return 0;

    switch(first->type){
        case LVAL_NUM:
            return first->num == second->num;

        case LVAL_ERR:
            return strcmp(first->err, second->err) == 0;

        case LVAL_SYM:
            return strcmp(first->sym, second->sym) == 0;

        case LVAL_STR:
            return strcmp(first->str, second->str) == 0;

        case LVAL_FUN:
            if (first->builtin || second->builtin) {
                return first->builtin == second->builtin;
            } else {
                return lval_eq(first->formals, second->formals)
                && lval_eq(first->body, second->body);
            }

        case LVAL_QEXP:
        case LVAL_SEXP:
            if(first->counter != second->counter)
                return 0;
            for(int i=0; i<first->counter; i++){
                if(!lval_eq(first->cell[i], second->cell[i]))
                    return 0;
            }
            return 1;
        break;
    }
    return 0;
}

lval *builtin_eq(lenv *e, lval *a){
    //LASSERT(a, a->counter == 2, "Function 'eq' got %d aruguments, should have taken 2", a->counter);

    // pivot value
    lval *first = lval_pop(a, 0);
    int res ;
    while(a->counter){
        lval *second = lval_pop(a, 0);
        res = lval_eq(first, second);
        if(!res)
            break;
    }
    lval_del(a);
    return create_lval_num(res);
}

lval *builtin_not_eq(lenv *e, lval *a){
    lval *v = builtin_eq(e, a);
    v->num = !v->num;
    return v;
}

lval *builtin_and(lenv *e, lval *a){
    int res;
    for(int i=0; i<a->counter; i++){
        LASSERT(a, a->cell[i]->type == LVAL_NUM, "Arguments of function 'and' must be num");
        res = a->cell[i]->num;
        if(!res)
            break;
    }
    return create_lval_num(res);
}

lval *builtin_or(lenv *e, lval *a){
    int res;
    for(int i=0; i<a->counter; i++){
        LASSERT(a, a->cell[i]->type == LVAL_NUM, "Arguments of function 'and' must be num");
        res = a->cell[i]->num;
        if(res)
            break;
    }
    return create_lval_num(res);
}

/*
    REPL functions
*/
lval *builtin_exit(lenv *e, lval *a){
    LASSERT(a, a->counter == 1, "Too many arguments for function 'exit'.\nGot %d, expected %d",
            a->counter, 0);
    LASSERT(a, a->cell[0]->type == LVAL_NUM, "Unexpected type.\nGot type %s, expected %s",
            ltype_name(a->cell[0]->type), ltype_name(LVAL_NUM));
    int code = a->cell[0]->num;
    lval_del(a);
    exit(code);
}

lval *builtin_print(lenv *e, lval *a){
    for(int i=0; i<a->counter; i++){
        lval_print(a->cell[i]);
        putchar(' ');
    }
    putchar('\n');
    lval_del(a);
    return create_lval_sexpr();
}

lval *builtin_error(lenv *e, lval *a){
    LASSERT(a, a->counter == 1, "Function 'error' takes exactly 1 string");
    LASSERT(a, a->cell[0]->type == LVAL_STR, "Got %s as argument, expected %s",
            ltype_name(a->cell[0]->type), ltype_name(LVAL_STR));
    lval *err = create_lval_err(a->cell[0]->str);
    lval_del(a);
    return err;
}

void lenv_add_builtin(lenv *e, char *name, lbuiltin func) {
    lval* k = create_lval_sym(name);
    lval* v = create_lval_fun(func);
    lenv_put(e, k, v);
    lval_del(k); lval_del(v);
    return;
}

void lenv_add_builtins(lenv* e) {
    /* List Functions */
    lenv_add_builtin(e, "list", builtin_list);
    lenv_add_builtin(e, "head", builtin_head);
    lenv_add_builtin(e, "tail", builtin_tail);
    lenv_add_builtin(e, "eval", builtin_eval);
    lenv_add_builtin(e, "join", builtin_join);
    lenv_add_builtin(e, "cons", builtin_cons);
    lenv_add_builtin(e, "len", builtin_len);
    lenv_add_builtin(e, "init", builtin_init);

    /* Mathematical Functions */
    lenv_add_builtin(e, "+", builtin_add);
    lenv_add_builtin(e, "-", builtin_sub);
    lenv_add_builtin(e, "*", builtin_mul);
    lenv_add_builtin(e, "/", builtin_div);
    lenv_add_builtin(e, "pow", builtin_pow);
    lenv_add_builtin(e, "max", builtin_max);
    lenv_add_builtin(e, "min", builtin_min);

    /* Variable functions */
    lenv_add_builtin(e, "def",  builtin_def);
    lenv_add_builtin(e, "=",   builtin_put);
    lenv_add_builtin(e, "show",  builtin_show);

    /* REPL functions */
    lenv_add_builtin(e, "exit",  builtin_exit);
    lenv_add_builtin(e, "print",  builtin_print);
    lenv_add_builtin(e, "error",  builtin_error);

    /* lambda functions */
    lenv_add_builtin(e, "lambda", builtin_lambda);
    lenv_add_builtin(e, "fun", builtin_fun);

    /* condition functions */
    lenv_add_builtin(e, ">", builtin_greater);
    lenv_add_builtin(e, ">=", builtin_greater_or_eq);
    lenv_add_builtin(e, "<", builtin_less);
    lenv_add_builtin(e, "<=", builtin_less_or_eq);
    lenv_add_builtin(e, "eq", builtin_eq);
    lenv_add_builtin(e, "neq", builtin_not_eq);
    lenv_add_builtin(e, "not", builtin_not);
        /* Logic operators */
    lenv_add_builtin(e, "and", builtin_and);
    lenv_add_builtin(e, "or", builtin_or);

    lenv_add_builtin(e, "if", builtin_if);

    /* load function */
    lenv_add_builtin(e, "load", builtin_load);
}

lval* builtin_op(lenv *e, lval* a, char *op) {
    /* Ensure all arguments are numbers */
    for (int i = 0; i < a->counter; i++) {
        if (a->cell[i]->type != LVAL_NUM) {
            lval_del(a);
            return create_lval_err("Cannot operate on non-number!");
        }
    }

    /* Pop the first element */
    lval* x = lval_pop(a, 0);

    /* If no arguments and sub then perform unary negation */
    if ((strcmp(op, "-") == 0) && a->counter == 0)
        x->num = -x->num;

    /* While there are still elements remaining */
    while (a->counter > 0) {

    /* Pop the next element */
    lval* y = lval_pop(a, 0);

    if (strcmp(op, "+") == 0) { x->num += y->num; }
    if (strcmp(op, "-") == 0) { x->num -= y->num; }
    if (strcmp(op, "*") == 0) { x->num *= y->num; }
    if (strcmp(op, "/") == 0) {
      if (y->num == 0) {
        lval_del(x); lval_del(y);
        x = create_lval_err("Division By Zero!"); break;
      }
      x->num /= y->num;
    }
    if (strcmp(op, "%") == 0) { x->num = (int)x->num % (int)y->num; }
    if (strcmp(op, "^") == 0) { x->num = pow(x->num, y->num); }
    if (strcmp(op, "max") == 0) { x->num = max(x->num, y->num); }
    if (strcmp(op, "min") == 0) { x->num = min(x->num, y->num); }

    lval_del(y);
    }

    lval_del(a);
    return x;
}

int main(int argc, char *argv[]){
    Number = mpc_new("number");
    String = mpc_new("string");
    Symbol = mpc_new("symbol");
    Comment = mpc_new("comment");
    Sexpr  = mpc_new("sexpr");
    Qexpr  = mpc_new("qexpr");
    Expr   = mpc_new("expr");
    Ecm  = mpc_new("ecm");

    mpca_lang(MPCA_LANG_DEFAULT,
      "                                                                    \
        number : /-?[0-9]+/ | '.' /-?[0-9]+/ ;                             \
        string  : /\"(\\\\.|[^\"])*\"/ ;                                   \
        symbol : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&%^]+/  ;                     \
        comment : /;[^\\r\\n]*/ ;                                          \
        sexpr  : '(' <expr>* ')' ;                                         \
        qexpr  : '{' <expr>* '}' ;                                         \
        expr   : <number> | <symbol> | <sexpr> | <qexpr> | <string> | <comment> ;                  \
        ecm  : /^/ <expr>* /$/ ;                                           \
      ",
      Number, String, Symbol, Comment, Sexpr, Qexpr, Expr, Ecm);

    // init the environment
    lenv *e = create_lenv();
    lenv_add_builtins(e);
    if(argc >= 2){
        for(int i=1; i<argc; i++){
            lval *args = lval_add(create_lval_sexpr(), create_lval_str(argv[i]));
            lval *x = builtin_load(e, args);
            if(x->type == LVAL_ERR)
                lval_println(x);
            lval_del(x);
        }
    }else{
        puts("Ecm version 0.2\n");
        char *input;
        while(true){
            input = readline(">");
            add_history(input);
            mpc_result_t r;
            if (mpc_parse("<stdin>", input, Ecm, &r)) {
                //print_ast(r);
                lval *lvalue = lval_eval(e, lval_read(r.output));
                lval_println(lvalue);
                lval_del(lvalue);
                mpc_ast_delete(r.output);
            } else {
              // If there is an error see the syntax tree
              print_ast(r);
            }
            free(input);
        }
    }
    // Cleans the arguments of parser mpc
    mpc_cleanup(8, Number, String, Symbol, Comment, Sexpr, Qexpr, Expr, Ecm);
    return EXIT_SUCCESS;
}
