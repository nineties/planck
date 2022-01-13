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
#include <libgen.h>

#define not_reachable() do { \
    fprintf(stderr, "Not reachable here: %s [%s:%d]\n", __func__, __FILE__, __LINE__); \
    exit(1); \
} while (0)

#define not_implemented() do { \
    fprintf(stderr, "Not implemented: %s [%s:%d]\n", __func__, __FILE__, __LINE__); \
    exit(1); \
} while (0)

#define max(a, b)   (((a) > (b)) ? (a) : (b))

typedef uint8_t byte_t;
typedef uint64_t uint_t;
typedef int64_t sint_t;

#define STACK_SIZE  (1024*1024)

// Operands
#define D_REG   0x80
#define D_ARG   0x90
#define D_UNIT  0xa0
#define D_NONE  0xc0
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
#define D_STR1  0xce
#define D_STR2  0xcf
#define D_STR4  0xd0
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
#define T_TUPLE 0xce
#define T_FUN   0xda
#define T_OBJ   0xff
// Sections
#define SEC_ID      0x00
#define SEC_FUN     0x01
#define SEC_VAR     0x02
#define SEC_EXPORT  0x03
#define SEC_IMPORT  0x04
// Instructions
#define I_NOP       0x00
#define I_PHI       0x01
#define I_MOVE      0x02
#define I_ADD       0x03
#define I_SUB       0x04
#define I_MUL       0x05
#define I_DIV       0x06
#define I_MOD       0x07
#define I_AND       0x08
#define I_OR        0x09
#define I_XOR       0x0a
#define I_EQ        0x0b
#define I_NE        0x0c
#define I_LT        0x0d
#define I_LE        0x0e
#define I_LCALL     0x20
#define I_ECALL     0x21
#define I_TUPLE     0x50
#define I_TUPLEAT   0x51
#define I_LOAD      0x60
#define I_STORE     0x61
#define I_GOTO      0x80
#define I_RETURN    0x81
#define I_IFTRUE    0x82
#define I_IFEQ      0x83
#define I_IFNE      0x84
#define I_IFLT      0x85
#define I_IFLE      0x86
// Values
#define V_NULL  0x00
#define V_UNIT  0x01
#define V_BOOL  0x02
#define V_U8    0x03
#define V_I8    0x04
#define V_U16   0x05
#define V_I16   0x06
#define V_U32   0x07
#define V_I32   0x08
#define V_U64   0x09
#define V_I64   0x0a
#define V_TUPLE 0x0b

typedef struct value {
    byte_t tag;
    union {
        bool b;
        uint64_t u;
        int64_t i;
        float f32;
        double f64;
        struct {
            size_t len;
            struct value *values;
        };
    };
} value;

static bool is_uint(value v) {
    return v.tag == V_U8 || v.tag == V_U16 || v.tag == V_U32 || v.tag == V_U64;
}

static bool is_sint(value v) {
    return v.tag == V_I8 || v.tag == V_I16 || v.tag == V_I32 || v.tag == V_I64;
}

typedef struct type {
    byte_t tag;   /* one of T_XXX */
    union {
        struct {
            struct type *ret;
            size_t n_params;
            struct type **params;
        };
        struct {
            size_t len;
            struct type **types;
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
        sint_t sint;
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
            operand cond0;
            operand cond1;
            uint_t ifthen;
            uint_t ifelse;
        };
        struct {
            union {
                operand lhs;
                uint_t store_idx;
            };
            union {
                operand rhs;    // lhs = rhs
                uint_t load_idx;
                struct {        // binary expression
                    operand arg0;
                    operand arg1;
                };
                struct {        // index access
                    operand arg;
                    uint_t index;
                };
                struct { // local and external function call and tuple
                    uint_t mod;
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
    char *comment;
} export_item;

typedef struct module {
    const char *name;
    size_t n_ids;
    char **ids;
    size_t n_func;
    function *funcs;
    size_t n_var;
    int startup;    /* -1 for none */
    struct {
        type *ty;
        value v;
    } *vars;
    size_t n_export;
    export_item *exports;
    size_t n_import;
    uint_t *import_ids;
    struct module **import_mods;
} module;

typedef struct {
    value *stack;
    value *sp;          /* stack pointer */
    size_t n_module;
    module **modules;
} interpreter;

// n-th local variable
#define LOCAL(bp, n)    (bp)[-(n)-1]
#define ARG(bp, n)      (bp)[n]

static value
operand_to_value(value *bp, operand *opd) {
    value v = { 0 };
    switch (opd->tag) {
    case D_UNIT:
        v.tag = V_UNIT;
        break;
    case D_TRUE:
        v.tag = V_BOOL;
        v.b = true;
        break;
    case D_FALSE:
        v.tag = V_BOOL;
        v.b = false;
        break;
    case D_U8:  v.tag = V_U8; v.u = opd->uint; break;
    case D_U16: v.tag = V_U16; v.u = opd->uint; break;
    case D_U32: v.tag = V_U32; v.u = opd->uint; break;
    case D_U64: v.tag = V_U64; v.u = opd->uint; break;
    case D_I8:  v.tag = V_I8; v.i = opd->sint; break;
    case D_I16: v.tag = V_I16; v.i = opd->sint; break;
    case D_I32: v.tag = V_I32; v.i = opd->sint; break;
    case D_I64: v.tag = V_I64; v.i = opd->sint; break;
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
lookup_id(module *mod, char *name) {
    for (int i = 0; i < mod->n_ids; i++) {
        if (strcmp(name, mod->ids[i]) == 0)
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
    switch (*p++) {
    case D_STR1: {
        size_t len = *p;
        *cur = p + len + 1;
        return strndup((char*)(p + 1), len);
    }
    case D_STR2: {
        size_t len = *(uint16_t*)p;
        *cur = p + len + 2;
        return strndup((char*)(p + 2), len);
    }
    case D_STR4: {
        size_t len = *(uint32_t*)p;
        *cur = p + len + 4;
        return strndup((char*)(p + 4), len);
    }
    }
    not_reachable();
    return 0;
}

static type *
decode_type(byte_t **cur) {
    byte_t code = *(*cur)++;
    if (0x80 <= code && code <= 0x8f) {
        type *t = calloc(1, sizeof(type));
        t->tag = T_TUPLE;
        t->len = code & 0x0f;
        t->types = calloc(t->len, sizeof(type*));
        for (int i = 0; i < t->len; i++)
            t->types[i] = decode_type(cur);
        return t;
    }
    switch (code) {
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
        case T_TUPLE: {
            type *t = calloc(1, sizeof(type));
            t->tag = T_TUPLE;
            t->len = *(*cur)++;
            t->types = calloc(t->len, sizeof(type*));
            for (int i = 0; i < t->len; i++)
                t->types[i] = decode_type(cur);
            return t;
        }
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
    case D_UNIT: opd->tag = D_UNIT; return;
    case D_TRUE: opd->tag = D_TRUE; return;
    case D_FALSE: opd->tag = D_FALSE; return;
    case D_U8:
        opd->tag = D_U8;
        opd->uint = *(*cur)++;
        return;
    case D_I8:
        opd->tag = D_I8;
        opd->sint = *(*cur)++;
        return;
    case D_U16:
        opd->tag = D_U16;
        opd->uint = *(uint16_t*)*cur;
        *cur += 2;
        return;
    case D_I16:
        opd->tag = D_I16;
        opd->sint = *(int16_t*)*cur;
        *cur += 2;
        return;
    case D_U32:
        opd->tag = D_U32;
        opd->uint = *(uint32_t*)*cur;
        *cur += 4;
        return;
    case D_I32:
        opd->tag = D_I32;
        opd->sint = *(int32_t*)*cur;
        *cur += 4;
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
    byte_t opcode = *(*cur)++;
    switch (opcode) {
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
    case I_AND:
    case I_OR:
    case I_XOR:
    case I_EQ:
    case I_NE:
    case I_LT:
    case I_LE:
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
    case I_ECALL:
        insn->tag = I_ECALL;
        decode_operand(fun, &insn->lhs, cur);
        insn->mod = decode_uint(cur);
        insn->fun = decode_uint(cur);
        insn->n_args = decode_uint(cur);
        insn->args = calloc(insn->n_args, sizeof(insn->args[0]));
        for (int i = 0; i < insn->n_args; i++)
            decode_operand(fun, &insn->args[i], cur);
        return;
    case I_TUPLE:
        insn->tag = I_TUPLE;
        decode_operand(fun, &insn->lhs, cur);
        insn->n_args = *(*cur)++;
        insn->args = calloc(insn->n_args, sizeof(insn->args[0]));
        for (int i = 0; i < insn->n_args; i++)
            decode_operand(fun, &insn->args[i], cur);
        return;
    case I_TUPLEAT:
        insn->tag = I_TUPLEAT;
        decode_operand(fun, &insn->lhs, cur);
        decode_operand(fun, &insn->arg, cur);
        insn->index = *(*cur)++;
        return;
    case I_LOAD:
        insn->tag = I_LOAD;
        decode_operand(fun, &insn->lhs, cur);
        insn->load_idx = decode_uint(cur);
        return;
    case I_STORE:
        insn->tag = I_STORE;
        insn->store_idx = decode_uint(cur);
        decode_operand(fun, &insn->rhs, cur);
        return;
    default:
        break;
    }
    if (0x30 <= opcode && opcode < 0x40) {
        insn->tag = I_TUPLE;
        decode_operand(fun, &insn->lhs, cur);
        insn->n_args = opcode & 0x0f;
        insn->args = calloc(insn->n_args, sizeof(insn->args[0]));
        for (int i = 0; i < insn->n_args; i++)
            decode_operand(fun, &insn->args[i], cur);
        return;
    }
    if (0x40 <= opcode && opcode < 0x50) {
        insn->tag = I_TUPLEAT;
        decode_operand(fun, &insn->lhs, cur);
        decode_operand(fun, &insn->arg, cur);
        insn->index = opcode & 0x0f;
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
        decode_operand(fun, &branch->cond0, cur);
        branch->ifthen = decode_uint(cur);
        branch->ifelse = decode_uint(cur);
        return;
    case I_IFEQ:
    case I_IFNE:
    case I_IFLT:
    case I_IFLE:
        branch->tag = *(*cur - 1);
        decode_operand(fun, &branch->cond0, cur);
        decode_operand(fun, &branch->cond1, cur);
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
    assert(fun->ty->tag == T_FUN);
    fun->n_blocks = decode_uint(cur);
    fun->blocks = calloc(fun->n_blocks, sizeof(basicblock));
    fun->n_locals = 0;
    for (int i = 0; i < fun->n_blocks; i++) {
        fun->blocks[i].index = i;
        decode_basicblock(fun, &fun->blocks[i], cur);
    }
}

static void
decode_section(module *mod, byte_t **cur) {
    switch (*(*cur)++) {
    case SEC_ID:
        mod->n_ids = decode_uint(cur);
        mod->ids = calloc(mod->n_ids, sizeof(char*));
        for (int i = 0; i < mod->n_ids; i++)
            mod->ids[i] = decode_str(cur);
        return;
    case SEC_FUN:
        mod->n_func = decode_uint(cur);
        mod->funcs = calloc(mod->n_func, sizeof(mod->funcs[0]));
        for (int i = 0; i < mod->n_func; i++)
            decode_function(&mod->funcs[i], cur);
        if (**cur == D_NONE) {
            mod->startup = -1;
            (*cur)++;
        } else {
            mod->startup = decode_uint(cur);
        }
        return;
    case SEC_VAR:
        mod->n_var = decode_uint(cur);
        mod->vars = calloc(mod->n_var, sizeof(mod->vars[0]));
        for (int i = 0; i < mod->n_var; i++)
            mod->vars[i].ty = decode_type(cur);
        return;
    case SEC_EXPORT:
        mod->n_export = decode_uint(cur);
        mod->exports = calloc(mod->n_export, sizeof(mod->exports[0]));
        for (int i = 0; i < mod->n_export; i++) {
            mod->exports[i].type = decode_uint(cur);
            mod->exports[i].id = decode_uint(cur);
            mod->exports[i].def = decode_uint(cur);
            mod->exports[i].comment = decode_str(cur);
        }
        return;
    case SEC_IMPORT:
        mod->n_import = decode_uint(cur);
        mod->import_ids = calloc(mod->n_import, sizeof(mod->import_ids[0]));
        mod->import_mods = calloc(mod->n_import, sizeof(mod->import_mods[0]));
        for (int i = 0; i < mod->n_import; i++)
            mod->import_ids[i] = decode_uint(cur);
        return;
    }
    not_reachable();
}

static module *
load_module(interpreter *interp, const char *name, const char *path) {
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
    module *mod = calloc(1, sizeof(module));
    mod->name = name;
    size_t n_sections = decode_uint(&cur);
    for (size_t i = 0; i < n_sections; i++)
        decode_section(mod, &cur);
    assert(cur == buffer + size);

    /* Add the module to interpreter */
    interp->modules = realloc(
            interp->modules,
            (interp->n_module + 1) * sizeof(interp->modules[0])
            );
    interp->modules[interp->n_module++] = mod;

    /* Load the dependent modules */
    char buf1[BUFSIZ], buf2[BUFSIZ];
    for (int i = 0; i < mod->n_import; i++) {
        strncpy(buf1, path, sizeof(buf1)-1);
        snprintf(buf2, sizeof(buf2), "%s/%s.pko",
                dirname(buf1), mod->ids[mod->import_ids[i]]);
        mod->import_mods[i] =
            load_module(interp, mod->ids[mod->import_ids[i]], buf2);
    }

    return mod;
}

static bool
check_type(type *ty, value v) {
    switch (v.tag) {
    case V_NULL: not_reachable();
    case V_UNIT:
        return ty->tag == T_TUPLE && ty->len == 0;
    case V_BOOL: return ty == &bool_type;
    case V_U8:   return ty == &u8_type;
    case V_I8:   return ty == &i8_type;
    case V_U16:  return ty == &u16_type;
    case V_I16:  return ty == &i16_type;
    case V_U32:  return ty == &u32_type;
    case V_I32:  return ty == &i32_type;
    case V_U64:  return ty == &u64_type;
    case V_I64:  return ty == &i64_type;
    case V_TUPLE:
        if (ty->tag != T_TUPLE) return false;
        if (ty->len != v.len) return false;
        for (int i = 0; i < v.len; i++)
            if (!check_type(ty->types[i], v.values[i]))
                return false;
        return true;
    }
    not_implemented();
}

#define CHECK_TYPE(ty, v) do { \
    if (!check_type(ty, v)) { \
        fprintf(stderr, "Type mismatch at %s [%s:%d]\n", __func__, __FILE__, __LINE__); \
        exit(1); \
    } \
} while (0)

static void
drop(value v) {
    switch(v.tag) {
    case V_TUPLE:
        for (int i = 0; i < v.len; i++)
            drop(v.values[i]);
        free(v.values);
        v.tag = V_NULL;
        break;
    default:
        break; /* do nothing */
    }
}

static void
move(value *bp, operand *lhs, value v) {
    switch (lhs->tag) {
    case D_REG: {
        value *p = &LOCAL(bp, lhs->reg);
        if (p->tag != V_NULL) drop(*p);
        *p = v;
        break;
    }
    default:
        not_implemented();
    }
}

static value
binexpr(byte_t op, value arg0, value arg1) {
    value v;
    switch (op) {
#define BINOP(op) \
        if (arg0.tag != arg1.tag) { \
            fprintf(stderr, "type mismatch\n"); \
            exit(1); \
        } \
        if (is_uint(arg0)) { \
            v.tag = arg0.tag; \
            v.u = arg0.u op arg1.u; \
            return v; \
        } \
        if (is_sint(arg0)) { \
            v.tag = arg0.tag; \
            v.i = arg0.i op arg1.i; \
            return v; \
        } \
        not_implemented();

    case I_ADD: BINOP(+)
    case I_SUB: BINOP(-)
    case I_MUL: BINOP(*)
    case I_DIV: BINOP(/)
    case I_MOD: BINOP(%)
#undef BINOP
#define LOGICOP(op) \
        if (arg0.tag != arg1.tag) { \
            fprintf(stderr, "type mismatch\n"); \
            exit(1); \
        } \
        if (is_uint(arg0)) { \
            v.tag = arg0.tag; \
            v.u = arg0.u op arg1.u; \
            return v; \
        } \
        if (is_sint(arg0)) { \
            v.tag = arg0.tag; \
            v.i = arg0.i op arg1.i; \
            return v; \
        } \
        if (arg0.tag == V_BOOL) {\
            v.tag = V_BOOL; \
            v.b = arg0.b op arg1.b; \
            return v; \
        } \
        not_implemented();

    case I_AND: LOGICOP(&)
    case I_OR:  LOGICOP(|)
    case I_XOR: LOGICOP(^)

#undef LOGICOP

#define COMPOP(op) \
        if (arg0.tag != arg1.tag) { \
            fprintf(stderr, "type mismatch\n"); \
            exit(1); \
        } \
        if (is_uint(arg0)) { \
            v.tag = V_BOOL; \
            v.b = arg0.u op arg1.u; \
            return v; \
        } \
        if (is_sint(arg0)) { \
            v.tag = V_BOOL; \
            v.b = arg0.i op arg1.i; \
            return v; \
        } \
        not_implemented();

    case I_EQ: COMPOP(==)
    case I_NE: COMPOP(!=)
    case I_LT: COMPOP(<)
    case I_LE: COMPOP(<=)
    case I_IFEQ: COMPOP(==)
    case I_IFNE: COMPOP(!=)
    case I_IFLT: COMPOP(<)
    case I_IFLE: COMPOP(<=)
#undef COMPOP

    default:
        not_implemented();
    }
}


static value
call(interpreter *interp, module *mod, function *fun) {
    value *bp = interp->sp;

    /* type check of arguments */
    for (int i = 0; i < fun->ty->n_params; i++) {
        if (!check_type(fun->ty->params[i], ARG(bp, i))) {
            fprintf(stderr, "type mismatch at %d-th argument\n", i);
            exit(1);
        }
    }

    /* allocate local variables and initialize them as V_NULL */
    interp->sp -= fun->n_locals;
    memset(interp->sp, 0, fun->n_locals*sizeof(value));

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
            case I_MOD:
            case I_AND:
            case I_OR:
            case I_XOR:
            case I_EQ:
            case I_NE:
            case I_LT:
            case I_LE:
            {
                move(bp, &insn->lhs,
                    binexpr(
                        insn->tag,
                        operand_to_value(bp, &insn->arg0),
                        operand_to_value(bp, &insn->arg1)
                    ));
                break;
            }
            case I_LCALL: {
                interp->sp -= insn->n_args; /* allocate space for arguments */
                for (int i = 0; i < insn->n_args; i++)
                    interp->sp[i] = operand_to_value(bp, &insn->args[i]);
                function *f = &mod->funcs[insn->fun];
                if (f->ty->n_params != insn->n_args) {
                    fprintf(stderr, "wrong number of arguments\n");
                    exit(1);
                }
                value ret = call(interp, mod, f);
                move(bp, &insn->lhs, ret);
                interp->sp += insn->n_args;
                break;
            }
            case I_ECALL: {
                interp->sp -= insn->n_args; /* allocate space for arguments */
                for (int i = 0; i < insn->n_args; i++)
                    interp->sp[i] = operand_to_value(bp, &insn->args[i]);
                if (insn->mod >= mod->n_import) not_reachable();
                module *m = mod->import_mods[insn->mod];
                /* lookup the function */
                function *f = NULL;
                for (int i = 0; i < m->n_export; i++) {
                    if (!strcmp(m->ids[m->exports[i].id], mod->ids[insn->fun]))
                        f = &m->funcs[m->exports[i].def];
                }
                if (!f) {
                    fprintf(stderr, "function %s is not found in module %s\n",
                            mod->ids[insn->fun], m->name);
                    exit(1);
                }
                if (f->ty->n_params != insn->n_args) {
                    fprintf(stderr, "wrong number of arguments\n");
                    exit(1);
                }
                value ret = call(interp, m, f);
                move(bp, &insn->lhs, ret);
                interp->sp += insn->n_args;
                break;
            }
            case I_TUPLE: {
                value t;
                t.tag = V_TUPLE;
                t.len = insn->n_args;
                t.values = calloc(t.len, sizeof(value));
                for (int i = 0; i < t.len; i++)
                    t.values[i] = operand_to_value(bp, &insn->args[i]);
                move(bp, &insn->lhs, t);
                break;
            }
            case I_TUPLEAT: {
                value v = operand_to_value(bp, &insn->arg);
                assert(v.tag  == V_TUPLE);
                assert(insn->index < v.len);
                move(bp, &insn->lhs, v.values[insn->index]);
                break;
            }
            case I_LOAD:
                assert(insn->load_idx < mod->n_var);
                move(bp, &insn->lhs, mod->vars[insn->load_idx].v);
                break;
            case I_STORE:
                assert(insn->store_idx < mod->n_var);
                value v = operand_to_value(bp, &insn->rhs);
                CHECK_TYPE(mod->vars[insn->store_idx].ty, v);
                mod->vars[insn->store_idx].v = v;
                break;
            case I_GOTO:
                prev = block;
                block = &fun->blocks[insn->next];
                break;
            case I_RETURN: {
                for (int i = 0; i < fun->n_locals; i++) {
                    if (insn->retval.tag == D_REG &&
                        insn->retval.reg == i)
                        continue;
                    drop(LOCAL(bp, i));
                }
                value ret = operand_to_value(bp, &insn->retval);
                CHECK_TYPE(fun->ty->ret, ret);
                return ret;
            }
            case I_IFTRUE: {
                prev = block;
                value v = operand_to_value(bp, &insn->cond0);
                if (v.tag != V_BOOL) not_reachable();
                block = &fun->blocks[v.b ? insn->ifthen : insn->ifelse];
                break;
            }
            case I_IFEQ:
            case I_IFNE:
            case I_IFLT:
            case I_IFLE: {
                prev = block;
                value v = binexpr(
                        insn->tag,
                        operand_to_value(bp, &insn->cond0),
                        operand_to_value(bp, &insn->cond1)
                    );
                if (v.tag != V_BOOL) not_reachable();
                block = &fun->blocks[v.b ? insn->ifthen : insn->ifelse];
                break;
            }
            default:
                printf("not implemented: %02x\n", insn->tag);
                not_implemented();
            }
        }
    }
}

static int
interpret(interpreter *interp, module *mod) {
    /* if the object file has startup function, call it */
    if (mod->startup >= 0) {
        function *startup_fun = &mod->funcs[mod->startup];
        call(interp, mod, startup_fun); /* todo type check */
    }

    uint_t main_id = lookup_id(mod, "main");
    function *main_fun = NULL;
    for (int i = 0; i < mod->n_export; i++) {
        if (mod->exports[i].id == main_id) {
            assert(mod->exports[i].type == 'F');
            main_fun = &mod->funcs[mod->exports[i].def];
            break;
        }
    }
    if (!main_fun) {
        fprintf(stderr, "\"main\" function is not found\n");
        exit(1);
    }
    if (main_fun->ty->tag != T_FUN ||
        main_fun->ty->n_params != 0 ||
        main_fun->ty->ret != &i32_type) {
        fprintf(stderr, "type of \"main\" must be () -> i32\n");
        exit(1);
    }

    value ret = call(interp, mod, main_fun);
    return (int) ret.i;
}

int
main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <object file>\n", argv[0]);
        return 1;
    }
    interpreter interp;
    interp.stack = calloc(STACK_SIZE, sizeof(value));
    interp.sp = interp.stack + STACK_SIZE;
    interp.n_module = 0;
    interp.modules = NULL;

    module *mod = load_module(&interp, "main", argv[1]);
    return interpret(&interp, mod);
}
