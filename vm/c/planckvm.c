/* planck - 
 * Copyright (C) 2021 nineties
 */

/* Reference implementation of PlanckVM in C
 * Error checking and memory free is omitted in favor of easy
 * to understand the logic.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#define not_reachable() do { \
    fprintf(stderr, "Not reachable here: %s\n", __func__); \
    exit(1); \
} while (0)

#define not_implemented() do { \
    fprintf(stderr, "Not implemented: %s\n", __func__); \
    exit(1); \
} while (0)

#define max(a, b)   (((a) > (b)) ? (a) : (b))

typedef uint8_t byte_t;
typedef uint64_t uint_t;
typedef int64_t sint_t;

#define STACK_SIZE  (1024*1024)

// Operands
#define D_REG   0x90
#define D_ARG   0xa0
#define D_TRUE  0xc1
#define D_FALSE 0xc2
#define D_U8    0xc3
#define D_I8    0xc4
#define D_U16   0xc5
#define D_I16   0xc6
#define D_U32   0xc7
#define D_I32   0xc8
#define D_U64   0xc9
#define D_I64   0xca
#define D_USER  0xdf
// Types
#define T_NEVER 0xc0
#define T_BOOL  0xc1
#define T_CHAR  0xc2
#define T_U8    0xc3
#define T_I8    0xc4
#define T_U16   0xc5
#define T_I16   0xc6
#define T_U32   0xc7
#define T_I32   0xc8
#define T_U64   0xc9
#define T_I64   0xca
#define T_F32   0xcb
#define T_F64   0xcc
#define T_STR   0xcd
#define T_FUN   0xda
#define T_OBJ   0xff
// Sections
#define SEC_ID      0x00
#define SEC_FUN     0x01
#define SEC_EXPORT  0x02
// Instructions
#define I_NOP       0x00
#define I_PHI       0x01
#define I_MOVE      0x02
#define I_ADD       0x03
#define I_SUB       0x04
#define I_MUL       0x05
#define I_DIV       0x06
#define I_MOD       0x07
#define I_LCALL     0x20    // local call
#define I_GOTO      0x80
#define I_RETURN    0x81
#define I_IFTRUE    0x82
// Values
#define V_BOOL  0x00
#define V_UINT  0x01
#define V_INT   0x02

typedef struct {
    byte_t tag;
    union {
        bool b;
        uint64_t u;
        int64_t i;
        float f32;
        double f64;
    };
} value;

typedef struct type {
    byte_t tag;   /* one of T_XXX */
    union {
        struct {
            struct type *ret;
            size_t n_params;
            struct type **params;
        };
    };
} type;

/* primitive types */
static type never_type = { T_NEVER };
static type bool_type = { T_BOOL };
static type char_type = { T_CHAR };
static type u8_type = { T_U8 };
static type i8_type = { T_I8 };
static type u16_type = { T_U16 };
static type i16_type = { T_I16 };
static type u32_type = { T_U32 };
static type i32_type = { T_I32 };
static type u64_type = { T_U64 };
static type i64_type = { T_I64 };
static type f32_type = { T_F32 };
static type f64_type = { T_F64 };
static type str_type = { T_STR };

typedef struct {
    byte_t tag;
    union {
        uint16_t reg;
        uint16_t arg;
        uint_t uint;
    };
} operand;

typedef struct {
    operand lhs;
    size_t n_rhs;
    uint_t *blocks;
    operand *rhs;
} phi_instruction;

typedef struct {
    byte_t tag;
    union {
        operand retval; // return
        uint_t next;    // goto
        struct {
            operand cond;
            uint_t ifthen;
            uint_t ifelse;
        };
        struct {
            operand lhs;
            union {
                operand rhs;    // lhs = rhs
                struct {        // binary expression
                    operand arg0;
                    operand arg1;
                };
                struct {    // local function call
                    uint_t fun;
                    size_t n_args;
                    operand *args;
                };
            };
        };
    };
} instruction;

typedef struct {
    uint_t index;
    size_t n_phi;
    phi_instruction *phis;
    size_t n_insn;
    instruction *insns;
} basicblock;

typedef struct {
    type *ty;
    size_t n_blocks; /* number of basic blocks */
    basicblock *blocks;
    size_t n_locals; /* number of local variables */
} function;

typedef struct {
    byte_t type;
    uint_t id;
    uint_t def;
} export_item;

typedef struct {
    size_t n_ids;
    char **ids;
    size_t n_func;
    function *funcs;
    size_t n_export;
    export_item *exports;
} object_file;

typedef struct {
    value *stack;
    value *sp;          /* stack pointer */
} interpreter;

// n-th local variable
#define LOCAL(bp, n)    (bp)[-(n)-1]
#define ARG(bp, n)      (bp)[n]

static value
operand_to_value(value *bp, operand *opd) {
    value v = { 0 };
    switch (opd->tag) {
    case D_TRUE:
        v.tag = V_BOOL;
        v.b = true;
        break;
    case D_FALSE:
        v.tag = V_BOOL;
        v.b = false;
        break;
    case D_U8:
        v.tag = V_UINT;
        v.u = opd->uint;
        break;
    case D_REG:
        v = LOCAL(bp, opd->reg);
        break;
    case D_ARG:
        v = ARG(bp, opd->arg);
        break;
    default:
        not_implemented();
    }
    return v;
}

static uint_t
lookup_id(object_file *obj, char *name) {
    for (int i = 0; i < obj->n_ids; i++) {
        if (strcmp(name, obj->ids[i]) == 0)
            return i;
    }
    fprintf(stderr, "ID \"%s\" is not found\n", name);
    exit(1);
}

static uint_t
decode_uint(byte_t **cur) {
    byte_t *p = *cur;
    if (p[0] < 128) {
        *cur = p + 1;
        return p[0];
    } else if (p[0] == D_U8) {
        *cur = p + 2;
        return p[1];
    } else if (p[0] == D_U16) {
        *cur = p + 3;
        return *(uint16_t*)(p + 1);
    } else if (p[0] == D_U32) {
        *cur = p + 5;
        return *(uint32_t*)(p + 1);
    } else if (p[0] == D_U64) {
        *cur = p + 9;
        return *(uint64_t*)(p + 1);
    }
    not_reachable();
    return 0;
}

static char *
decode_str(byte_t **cur) {
    byte_t *p = *cur;
    if (((*p) & 0xe0) == 0xa0) {
        size_t len = (*p) & 0x1f;
        *cur = p + len + 1;
        return strndup((char*)(p + 1), len);
    }
    not_reachable();
    return 0;
}

static type *
decode_type(byte_t **cur) {
    switch (*(*cur)++) {
        case T_NEVER: return &never_type;
        case T_BOOL: return &bool_type;
        case T_CHAR: return &char_type;
        case T_U8: return &u8_type;
        case T_I8: return &i8_type;
        case T_U16: return &u16_type;
        case T_I16: return &i16_type;
        case T_U32: return &u32_type;
        case T_I32: return &i32_type;
        case T_U64: return &u64_type;
        case T_I64: return &i64_type;
        case T_F32: return &f32_type;
        case T_F64: return &f64_type;
        case T_STR: return &str_type;
        case T_FUN: {
            type *fun = calloc(1, sizeof(type));
            fun->tag = T_FUN;
            fun->ret = decode_type(cur);
            fun->n_params = decode_uint(cur);
            fun->params = calloc(fun->n_params, sizeof(fun->params[0]));
            for (int i = 0; i < fun->n_params; i++)
                fun->params[i] = decode_type(cur);
            return fun;
        }
    }
    not_implemented();
    return NULL;
}

static void
decode_operand(function *fun, operand *opd, byte_t **cur)
{
    byte_t *p = *cur;
    if (*p < 128) {
        opd->tag = D_U8;
        opd->uint = *p;
        *cur = p + 1;
        return;
    }
    if (*p < 0x90) {
        opd->tag = D_REG;
        opd->reg = *p & 0x0f;
        fun->n_locals = max(fun->n_locals, opd->reg + 1);
        *cur = p + 1;
        return;
    }
    if (*p < 0xa0) {
        opd->tag = D_ARG;
        opd->arg = *p & 0x0f;
        *cur = p + 1;
        return;
    }
    switch (*(*cur)++) {
    case D_TRUE: opd->tag = D_TRUE; return;
    case D_FALSE: opd->tag = D_FALSE; return;
    case D_U8:
        opd->tag = D_U8;
        opd->uint = *(*cur)++;
        return;
    case D_U16:
        opd->tag = D_U16;
        opd->uint = *(uint16_t*)*cur;
        *cur += 2;
        return;
    default:
        not_implemented();
    }
}

static void
decode_phi_instruction(function *fun, phi_instruction *phi, byte_t **cur) {
    assert((*cur)[0] == I_PHI);
    (*cur)++;
    decode_operand(fun, &phi->lhs, cur);
    phi->n_rhs = decode_uint(cur);
    phi->blocks = calloc(phi->n_rhs, sizeof(phi->blocks[0]));
    phi->rhs = calloc(phi->n_rhs, sizeof(phi->rhs[0]));
    for (int i = 0; i < phi->n_rhs; i++) {
        phi->blocks[i] = decode_uint(cur);
        decode_operand(fun, &phi->rhs[i], cur);
    }
}

static void
decode_instruction(function *fun, instruction *insn, byte_t **cur) {
    switch (*(*cur)++) {
    case I_NOP:
        insn->tag = I_NOP;
        return;
    case I_MOVE:
        insn->tag = I_MOVE;
        decode_operand(fun, &insn->lhs, cur);
        decode_operand(fun, &insn->rhs, cur);
        return;
    case I_ADD:
    case I_SUB:
    case I_MUL:
    case I_DIV:
    case I_MOD:
        insn->tag = *(*cur - 1);
        decode_operand(fun, &insn->lhs, cur);
        decode_operand(fun, &insn->arg0, cur);
        decode_operand(fun, &insn->arg1, cur);
        return;
    case I_LCALL:
        insn->tag = I_LCALL;
        decode_operand(fun, &insn->lhs, cur);
        insn->fun = decode_uint(cur);
        insn->n_args = decode_uint(cur);
        insn->args = calloc(insn->n_args, sizeof(insn->args[0]));
        for (int i = 0; i < insn->n_args; i++)
            decode_operand(fun, &insn->args[i], cur);
        return;
    }
    not_implemented();
}

static void
decode_branch_instruction(function *fun, instruction *branch, byte_t **cur) {
    switch (*(*cur)++) {
    case I_GOTO:
        branch->tag = I_GOTO;
        branch->next = decode_uint(cur);
        return;
    case I_RETURN:
        branch->tag = I_RETURN;
        decode_operand(fun, &branch->retval, cur);
        return;
    case I_IFTRUE:
        branch->tag = I_IFTRUE;
        decode_operand(fun, &branch->cond, cur);
        branch->ifthen = decode_uint(cur);
        branch->ifelse = decode_uint(cur);
        return;
    }
    not_reachable();
}

static void
decode_basicblock(function *fun, basicblock *block, byte_t **cur) {
    block->n_phi = decode_uint(cur);
    block->phis = calloc(block->n_phi, sizeof(block->phis[0]));
    for (int i = 0; i < block->n_phi; i++)
        decode_phi_instruction(fun, &block->phis[i], cur);
    block->n_insn = decode_uint(cur);
    block->insns = calloc(block->n_insn + 1, sizeof(block->insns[0]));
    for (int i = 0; i < block->n_insn; i++)
        decode_instruction(fun, &block->insns[i], cur);
    decode_branch_instruction(fun, &block->insns[block->n_insn], cur);
}

static void
decode_function(function *fun, byte_t **cur) {
    fun->ty = decode_type(cur);
    fun->n_blocks = decode_uint(cur);
    fun->blocks = calloc(fun->n_blocks, sizeof(basicblock));
    fun->n_locals = 0;
    for (int i = 0; i < fun->n_blocks; i++) {
        fun->blocks[i].index = i;
        decode_basicblock(fun, &fun->blocks[i], cur);
    }
}

static void
decode_section(object_file *obj, byte_t **cur) {
    switch (*(*cur)++) {
    case SEC_ID:
        obj->n_ids = decode_uint(cur);
        obj->ids = calloc(obj->n_ids, sizeof(char*));
        for (int i = 0; i < obj->n_ids; i++)
            obj->ids[i] = decode_str(cur);
        return;
    case SEC_FUN:
        obj->n_func = decode_uint(cur);
        obj->funcs = calloc(obj->n_func, sizeof(obj->funcs[0]));
        for (int i = 0; i < obj->n_func; i++)
            decode_function(&obj->funcs[i], cur);
        return;
    case SEC_EXPORT:
        obj->n_export = decode_uint(cur);
        obj->exports = calloc(obj->n_export, sizeof(obj->exports[0]));
        for (int i = 0; i < obj->n_export; i++) {
            obj->exports[i].type = decode_uint(cur);
            obj->exports[i].id = decode_uint(cur);
            obj->exports[i].def = decode_uint(cur);
        }
        return;
    }
    not_reachable();
}

static object_file *
load_object_file(const char *path) {
    FILE *fp = fopen(path, "rb");
    fpos_t pos;
    fseek(fp, 0, SEEK_END);
    fgetpos(fp, &pos);
    fseek(fp, 0, SEEK_SET);
    size_t size = pos.__pos;
    byte_t *buffer = calloc(1, size);
    fread(buffer, 1, size, fp);
    fclose(fp);

    assert(buffer[0] == D_USER);
    assert(buffer[1] == T_OBJ);

    byte_t *cur = buffer + 2;
    object_file *obj = calloc(1, sizeof(object_file));
    size_t n_sections = decode_uint(&cur);
    for (size_t i = 0; i < n_sections; i++)
        decode_section(obj, &cur);
    assert(cur == buffer + size);

    return obj;
}

static void
move(value *bp, operand *lhs, value v) {
    switch (lhs->tag) {
    case D_REG:
        LOCAL(bp, lhs->reg) = v;
        break;
    default:
        not_implemented();
    }
}

static value
binexpr(byte_t op, value arg0, value arg1) {
    value v;
    switch (op) {
    case I_ADD:
        if (arg0.tag == V_UINT && arg1.tag == V_UINT) {
            v.tag = V_UINT;
            v.u = arg0.u + arg1.u;
            return v;
        }
        not_implemented();
    case I_SUB:
        if (arg0.tag == V_UINT && arg1.tag == V_UINT) {
            v.tag = V_UINT;
            v.u = arg0.u - arg1.u;
            return v;
        }
        not_implemented();
    case I_MUL:
        if (arg0.tag == V_UINT && arg1.tag == V_UINT) {
            v.tag = V_UINT;
            v.u = arg0.u * arg1.u;
            return v;
        }
        not_implemented();
    case I_DIV:
        if (arg0.tag == V_UINT && arg1.tag == V_UINT) {
            v.tag = V_UINT;
            v.u = arg0.u / arg1.u;
            return v;
        }
        not_implemented();
    case I_MOD:
        if (arg0.tag == V_UINT && arg1.tag == V_UINT) {
            v.tag = V_UINT;
            v.u = arg0.u % arg1.u;
            return v;
        }
        not_implemented();
    default:
        not_implemented();
    }
}


static value
call(interpreter *interp, object_file *obj, function *fun) {
    value *bp = interp->sp;
    interp->sp -= fun->n_locals; /* allocate space for local variables */
    basicblock *prev = NULL;
    basicblock *block = &fun->blocks[0];

    /* entry block does not have phi */
    assert(block->n_phi == 0);

    for (;;) {
        /* interpret phi functions */
        for (int i = 0; i < block->n_phi; i++) {
            phi_instruction *phi = &block->phis[i];
            for (int j = 0; j < phi->n_rhs; j++) {
                if (prev->index == phi->blocks[j]) {
                    move(bp, &phi->lhs, operand_to_value(bp, &phi->rhs[j]));
                    break;
                }
            }
        }

        size_t n_insn = block->n_insn;
        for (int i = 0; i < n_insn + 1; i++) {
            instruction *insn = &block->insns[i];
            switch (insn->tag) {
            case I_NOP:
                /* do nothing */
                break;
            case I_MOVE:
                move(bp, &insn->lhs, operand_to_value(bp, &insn->rhs));
                break;
            case I_ADD:
            case I_SUB:
            case I_MUL:
            case I_DIV:
            case I_MOD: {
                move(bp, &insn->lhs,
                    binexpr(
                        insn->tag,
                        operand_to_value(bp, &insn->arg0),
                        operand_to_value(bp, &insn->arg1)
                    ));
                break;
            }
            case I_LCALL:
                interp->sp -= insn->n_args; /* allocate space for arguments */
                for (int i = 0; i < insn->n_args; i++)
                    interp->sp[i] = operand_to_value(bp, &insn->args[i]);
                value ret = call(interp, obj, &obj->funcs[insn->fun]);
                move(bp, &insn->lhs, ret);
                interp->sp += insn->n_args;
                break;
            case I_GOTO:
                prev = block;
                block = &fun->blocks[insn->next];
                break;
            case I_RETURN:
                return operand_to_value(bp, &insn->retval);
            case I_IFTRUE:
                prev = block;
                value v = operand_to_value(bp, &insn->cond);
                if (v.tag != V_BOOL) not_reachable();
                block = &fun->blocks[v.b ? insn->ifthen : insn->ifelse];
                break;
            default:
                printf("not implemented: %02x\n", insn->tag);
                not_implemented();
            }
        }
    }
}

static int
interpret(object_file *obj) {
    interpreter interp;
    interp.stack = calloc(STACK_SIZE, sizeof(value));
    interp.sp = interp.stack + STACK_SIZE;

    uint_t main_id = lookup_id(obj, "main");
    function *main_fun = NULL;
    for (int i = 0; i < obj->n_export; i++) {
        if (obj->exports[i].id == main_id) {
            assert(obj->exports[i].type == 'F');
            main_fun = &obj->funcs[obj->exports[i].def];
            break;
        }
    }
    if (!main_fun) {
        fprintf(stderr, "\"main\" function is not found\n");
        exit(1);
    }
    value ret = call(&interp, obj, main_fun);
    return (int) ret.i;
}

int
main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <object file>\n", argv[0]);
        return 1;
    }
    object_file *obj = load_object_file(argv[1]);
    return interpret(obj);
}
