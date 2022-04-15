/*
 * QuickJS Javascript Engine
 *
 * Copyright (c) 2017-2021 Fabrice Bellard
 * Copyright (c) 2017-2021 Charlie Gordon
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>
#include <sys/time.h>
#include <time.h>
#include <fenv.h>
#include <math.h>
#if defined(__APPLE__)
#include <malloc/malloc.h>
#elif defined(__linux__)
#include <malloc.h>
#elif defined(__FreeBSD__)
#include <malloc_np.h>
#endif

#include "cutils.h"
#include "list.h"
#include "quickjs.h"
#include "libregexp.h"
#include "libbf.h"

#define OPTIMIZE         1
#define SHORT_OPCODES    1
#if defined(EMSCRIPTEN)
#define DIRECT_DISPATCH  0
#else
#define DIRECT_DISPATCH  1
#endif

#if defined(__APPLE__)
#define MALLOC_OVERHEAD  0
#else
#define MALLOC_OVERHEAD  8
#endif

#if !defined(_WIN32)
/* define it if printf uses the RNDN rounding mode instead of RNDNA */
#define CONFIG_PRINTF_RNDN
#endif

/* define to include Atomics.* operations which depend on the OS
   threads */
#if !defined(EMSCRIPTEN)
#define CONFIG_ATOMICS
#endif

#if !defined(EMSCRIPTEN)
/* enable stack limitation */
#define CONFIG_STACK_CHECK
#endif


/* dump object free */
//#define DUMP_FREE
//#define DUMP_CLOSURE
/* dump the bytecode of the compiled functions: combination of bits
   1: dump pass 3 final byte code
   2: dump pass 2 code
   4: dump pass 1 code
   8: dump stdlib functions
  16: dump bytecode in hex
  32: dump line number table
 */
//#define DUMP_BYTECODE  (1)
/* dump the occurence of the automatic GC */
//#define DUMP_GC
/* dump objects freed by the garbage collector */
//#define DUMP_GC_FREE
/* dump objects leaking when freeing the runtime */
//#define DUMP_LEAKS  1
/* dump memory usage before running the garbage collector */
//#define DUMP_MEM
//#define DUMP_OBJECTS    /* dump objects in JS_FreeContext */
//#define DUMP_ATOMS      /* dump atoms in JS_FreeContext */
//#define DUMP_SHAPES     /* dump shapes in JS_FreeContext */
//#define DUMP_MODULE_RESOLVE
//#define DUMP_PROMISE
//#define DUMP_READ_OBJECT

/* test the GC by forcing it before each object allocation */
//#define FORCE_GC_AT_MALLOC

#ifdef CONFIG_ATOMICS
#include <pthread.h>
#include <stdatomic.h>
#include <errno.h>
#endif

enum {
    /* classid tag        */    /* union usage   | properties */
    JS_CLASS_OBJECT = 1,        /* must be first */
    JS_CLASS_ARRAY,             /* u.array       | length */
    JS_CLASS_ERROR,
    JS_CLASS_NUMBER,            /* u.object_data */
    JS_CLASS_STRING,            /* u.object_data */
    JS_CLASS_BOOLEAN,           /* u.object_data */
    JS_CLASS_SYMBOL,            /* u.object_data */
    JS_CLASS_ARGUMENTS,         /* u.array       | length */
    JS_CLASS_MAPPED_ARGUMENTS,  /*               | length */
    JS_CLASS_DATE,              /* u.object_data */
    JS_CLASS_MODULE_NS,
    JS_CLASS_C_FUNCTION,        /* u.cfunc */
    JS_CLASS_BYTECODE_FUNCTION, /* u.func */
    JS_CLASS_BOUND_FUNCTION,    /* u.bound_function */
    JS_CLASS_C_FUNCTION_DATA,   /* u.c_function_data_record */
    JS_CLASS_GENERATOR_FUNCTION, /* u.func */
    JS_CLASS_FOR_IN_ITERATOR,   /* u.for_in_iterator */
    JS_CLASS_REGEXP,            /* u.regexp */
    JS_CLASS_ARRAY_BUFFER,      /* u.array_buffer */
    JS_CLASS_SHARED_ARRAY_BUFFER, /* u.array_buffer */
    JS_CLASS_UINT8C_ARRAY,      /* u.array (typed_array) */
    JS_CLASS_INT8_ARRAY,        /* u.array (typed_array) */
    JS_CLASS_UINT8_ARRAY,       /* u.array (typed_array) */
    JS_CLASS_INT16_ARRAY,       /* u.array (typed_array) */
    JS_CLASS_UINT16_ARRAY,      /* u.array (typed_array) */
    JS_CLASS_INT32_ARRAY,       /* u.array (typed_array) */
    JS_CLASS_UINT32_ARRAY,      /* u.array (typed_array) */
    JS_CLASS_BIG_INT64_ARRAY,   /* u.array (typed_array) */
    JS_CLASS_BIG_UINT64_ARRAY,  /* u.array (typed_array) */
    JS_CLASS_FLOAT32_ARRAY,     /* u.array (typed_array) */
    JS_CLASS_FLOAT64_ARRAY,     /* u.array (typed_array) */
    JS_CLASS_DATAVIEW,          /* u.typed_array */
    JS_CLASS_BIG_INT,           /* u.object_data */
    JS_CLASS_BIG_FLOAT,         /* u.object_data */
    JS_CLASS_FLOAT_ENV,         /* u.float_env */
    JS_CLASS_BIG_DECIMAL,       /* u.object_data */
    JS_CLASS_OPERATOR_SET,      /* u.operator_set */
    JS_CLASS_MAP,               /* u.map_state */
    JS_CLASS_SET,               /* u.map_state */
    JS_CLASS_WEAKMAP,           /* u.map_state */
    JS_CLASS_WEAKSET,           /* u.map_state */
    JS_CLASS_MAP_ITERATOR,      /* u.map_iterator_data */
    JS_CLASS_SET_ITERATOR,      /* u.map_iterator_data */
    JS_CLASS_ARRAY_ITERATOR,    /* u.array_iterator_data */
    JS_CLASS_STRING_ITERATOR,   /* u.array_iterator_data */
    JS_CLASS_REGEXP_STRING_ITERATOR,   /* u.regexp_string_iterator_data */
    JS_CLASS_GENERATOR,         /* u.generator_data */
    JS_CLASS_PROXY,             /* u.proxy_data */
    JS_CLASS_PROMISE,           /* u.promise_data */
    JS_CLASS_PROMISE_RESOLVE_FUNCTION,  /* u.promise_function_data */
    JS_CLASS_PROMISE_REJECT_FUNCTION,   /* u.promise_function_data */
    JS_CLASS_ASYNC_FUNCTION,            /* u.func */
    JS_CLASS_ASYNC_FUNCTION_RESOLVE,    /* u.async_function_data */
    JS_CLASS_ASYNC_FUNCTION_REJECT,     /* u.async_function_data */
    JS_CLASS_ASYNC_FROM_SYNC_ITERATOR,  /* u.async_from_sync_iterator_data */
    JS_CLASS_ASYNC_GENERATOR_FUNCTION,  /* u.func */
    JS_CLASS_ASYNC_GENERATOR,   /* u.async_generator_data */

    JS_CLASS_INIT_COUNT, /* last entry for predefined classes */
};

/* number of typed array types */
#define JS_TYPED_ARRAY_COUNT  (JS_CLASS_FLOAT64_ARRAY - JS_CLASS_UINT8C_ARRAY + 1)
static uint8_t const typed_array_size_log2[JS_TYPED_ARRAY_COUNT];
#define typed_array_size_log2(classid)  (typed_array_size_log2[(classid)- JS_CLASS_UINT8C_ARRAY])

typedef enum JSErrorEnum {
    JS_EVAL_ERROR,
    JS_RANGE_ERROR,
    JS_REFERENCE_ERROR,
    JS_SYNTAX_ERROR,
    JS_TYPE_ERROR,
    JS_URI_ERROR,
    JS_INTERNAL_ERROR,
    JS_AGGREGATE_ERROR,

    JS_NATIVE_ERROR_COUNT, /* number of different NativeError objects */
} JSErrorEnum;

#define JS_MAX_LOCAL_VARS 65536
#define JS_STACK_SIZE_MAX 65534
#define JS_STRING_LEN_MAX ((1 << 30) - 1)

#define __exception __attribute__((warn_unused_result))

typedef struct JSShape JSShape;
typedef struct JSString JSString;
typedef struct JSString JSAtomStruct;

typedef enum {
    JS_GC_PHASE_NONE,
    JS_GC_PHASE_DECREF,
    JS_GC_PHASE_REMOVE_CYCLES,
} JSGCPhaseEnum;

typedef enum OPCodeEnum OPCodeEnum;

/* function pointers are used for numeric operations so that it is
   possible to remove some numeric types */
typedef struct {
    JSValue (*to_string)(JSContext *ctx, JSValueConst val);
    JSValue (*from_string)(JSContext *ctx, const char *buf,
                           int radix, int flags, slimb_t *pexponent);
    int (*unary_arith)(JSContext *ctx,
                       JSValue *pres, OPCodeEnum op, JSValue op1);
    int (*binary_arith)(JSContext *ctx, OPCodeEnum op,
                        JSValue *pres, JSValue op1, JSValue op2);
    int (*compare)(JSContext *ctx, OPCodeEnum op,
                   JSValue op1, JSValue op2);
    /* only for bigfloat: */
    JSValue (*mul_pow10_to_float64)(JSContext *ctx, const bf_t *a,
                                    int64_t exponent);
    int (*mul_pow10)(JSContext *ctx, JSValue *sp);
} JSNumericOperations;

struct JSRuntime {
    JSMallocFunctions mf;
    JSMallocState malloc_state;
    const char *rt_info;

    int atom_hash_size; /* power of two */
    int atom_count;
    int atom_size;
    int atom_count_resize; /* resize hash table at this count */
    uint32_t *atom_hash;
    JSAtomStruct **atom_array;
    int atom_free_index; /* 0 = none */

    int class_count;    /* size of class_array */
    JSClass *class_array;

    struct list_head context_list; /* list of JSContext.link */
    /* list of JSGCObjectHeader.link. List of allocated GC objects (used
       by the garbage collector) */
    struct list_head gc_obj_list;
    /* list of JSGCObjectHeader.link. Used during JS_FreeValueRT() */
    struct list_head gc_zero_ref_count_list;
    struct list_head tmp_obj_list; /* used during GC */
    JSGCPhaseEnum gc_phase : 8;
    size_t malloc_gc_threshold;
#ifdef DUMP_LEAKS
    struct list_head string_list; /* list of JSString.link */
#endif
    /* stack limitation */
    uintptr_t stack_size; /* in bytes, 0 if no limit */
    uintptr_t stack_top;
    uintptr_t stack_limit; /* lower stack limit */

    JSValue current_exception;
    /* true if inside an out of memory error, to avoid recursing */
    BOOL in_out_of_memory : 8;

    struct JSStackFrame *current_stack_frame;

    JSInterruptHandler *interrupt_handler;
    void *interrupt_opaque;

    JSHostPromiseRejectionTracker *host_promise_rejection_tracker;
    void *host_promise_rejection_tracker_opaque;

    struct list_head job_list; /* list of JSJobEntry.link */

    JSModuleNormalizeFunc *module_normalize_func;
    JSModuleLoaderFunc *module_loader_func;
    void *module_loader_opaque;

    BOOL can_block : 8; /* TRUE if Atomics.wait can block */
    /* used to allocate, free and clone SharedArrayBuffers */
    JSSharedArrayBufferFunctions sab_funcs;

    /* Shape hash table */
    int shape_hash_bits;
    int shape_hash_size;
    int shape_hash_count; /* number of hashed shapes */
    JSShape **shape_hash;
    bf_context_t bf_ctx;
    JSNumericOperations bigint_ops;
    JSNumericOperations bigfloat_ops;
    JSNumericOperations bigdecimal_ops;
    uint32_t operator_count;
    void *user_opaque;
};

struct JSClass {
    uint32_t class_id; /* 0 means free entry */
    JSAtom class_name;
    JSClassFinalizer *finalizer;
    JSClassGCMark *gc_mark;
    JSClassCall *call;
    /* pointers for exotic behavior, can be NULL if none are present */
    const JSClassExoticMethods *exotic;
};

#define JS_MODE_STRICT (1 << 0)
#define JS_MODE_STRIP  (1 << 1)
#define JS_MODE_MATH   (1 << 2)

typedef struct JSStackFrame {
    struct JSStackFrame *prev_frame; /* NULL if first stack frame */
    JSValue cur_func; /* current function, JS_UNDEFINED if the frame is detached */
    JSValue *arg_buf; /* arguments */
    JSValue *var_buf; /* variables */
    struct list_head var_ref_list; /* list of JSVarRef.link */
    const uint8_t *cur_pc; /* only used in bytecode functions : PC of the
                        instruction after the call */
    int arg_count;
    int js_mode; /* 0 or JS_MODE_MATH for C functions */
    /* only used in generators. Current stack pointer value. NULL if
       the function is running. */
    JSValue *cur_sp;
} JSStackFrame;

typedef enum {
    JS_GC_OBJ_TYPE_JS_OBJECT,
    JS_GC_OBJ_TYPE_FUNCTION_BYTECODE,
    JS_GC_OBJ_TYPE_SHAPE,
    JS_GC_OBJ_TYPE_VAR_REF,
    JS_GC_OBJ_TYPE_ASYNC_FUNCTION,
    JS_GC_OBJ_TYPE_JS_CONTEXT,
} JSGCObjectTypeEnum;

/* header for GC objects. GC objects are C data structures with a
   reference count that can reference other GC objects. JS Objects are
   a particular type of GC object. */
struct JSGCObjectHeader {
    int ref_count; /* must come first, 32-bit */
    JSGCObjectTypeEnum gc_obj_type : 4;
    uint8_t mark : 4; /* used by the GC */
    uint8_t dummy1; /* not used by the GC */
    uint16_t dummy2; /* not used by the GC */
    struct list_head link;
};

typedef struct JSVarRef {
    union {
        JSGCObjectHeader header; /* must come first */
        struct {
            int __gc_ref_count; /* corresponds to header.ref_count */
            uint8_t __gc_mark; /* corresponds to header.mark/gc_obj_type */

            /* 0 : the JSVarRef is on the stack. header.link is an element
               of JSStackFrame.var_ref_list.
               1 : the JSVarRef is detached. header.link has the normal meanning
            */
            uint8_t is_detached : 1;
            uint8_t is_arg : 1;
            uint16_t var_idx; /* index of the corresponding function variable on
                                 the stack */
        };
    };
    JSValue *pvalue; /* pointer to the value, either on the stack or
                        to 'value' */
    JSValue value; /* used when the variable is no longer on the stack */
} JSVarRef;

typedef struct JSFloatEnv {
    limb_t prec;
    bf_flags_t flags;
    unsigned int status;
} JSFloatEnv;

/* the same structure is used for big integers and big floats. Big
   integers are never infinite or NaNs */
typedef struct JSBigFloat {
    JSRefCountHeader header; /* must come first, 32-bit */
    bf_t num;
} JSBigFloat;

typedef struct JSBigDecimal {
    JSRefCountHeader header; /* must come first, 32-bit */
    bfdec_t num;
} JSBigDecimal;

typedef enum {
    JS_AUTOINIT_ID_PROTOTYPE,
    JS_AUTOINIT_ID_MODULE_NS,
    JS_AUTOINIT_ID_PROP,
} JSAutoInitIDEnum;

/* must be large enough to have a negligible runtime cost and small
   enough to call the interrupt callback often. */
#define JS_INTERRUPT_COUNTER_INIT 10000

struct JSContext {
    JSGCObjectHeader header; /* must come first */
    JSRuntime *rt;
    struct list_head link;

    uint16_t binary_object_count;
    int binary_object_size;

    JSShape *array_shape;   /* initial shape for Array objects */

    JSValue *class_proto;
    JSValue function_proto;
    JSValue function_ctor;
    JSValue array_ctor;
    JSValue regexp_ctor;
    JSValue promise_ctor;
    JSValue native_error_proto[JS_NATIVE_ERROR_COUNT];
    JSValue iterator_proto;
    JSValue async_iterator_proto;
    JSValue array_proto_values;
    JSValue throw_type_error;
    JSValue eval_obj;

    JSValue global_obj; /* global object */
    JSValue global_var_obj; /* contains the global let/const definitions */

    uint64_t random_state;
    bf_context_t *bf_ctx;   /* points to rt->bf_ctx, shared by all contexts */
    JSFloatEnv fp_env; /* global FP environment */
    BOOL bignum_ext : 8; /* enable math mode */
    BOOL allow_operator_overloading : 8;

    /* when the counter reaches zero, JSRutime.interrupt_handler is called */
    int interrupt_counter;
    BOOL is_error_property_enabled;

    struct list_head loaded_modules; /* list of JSModuleDef.link */

    /* if NULL, RegExp compilation is not supported */
    JSValue (*compile_regexp)(JSContext *ctx, JSValueConst pattern,
                              JSValueConst flags);
    /* if NULL, eval is not supported */
    JSValue (*eval_internal)(JSContext *ctx, JSValueConst this_obj,
                             const char *input, size_t input_len,
                             const char *filename, int flags, int scope_idx);
    void *user_opaque;
};

typedef union JSFloat64Union {
    double d;
    uint64_t u64;
    uint32_t u32[2];
} JSFloat64Union;

enum {
    JS_ATOM_TYPE_STRING = 1,
    JS_ATOM_TYPE_GLOBAL_SYMBOL,
    JS_ATOM_TYPE_SYMBOL,
    JS_ATOM_TYPE_PRIVATE,
};

enum {
    JS_ATOM_HASH_SYMBOL,
    JS_ATOM_HASH_PRIVATE,
};

typedef enum {
    JS_ATOM_KIND_STRING,
    JS_ATOM_KIND_SYMBOL,
    JS_ATOM_KIND_PRIVATE,
} JSAtomKindEnum;

#define JS_ATOM_HASH_MASK  ((1 << 30) - 1)

struct JSString {
    JSRefCountHeader header; /* must come first, 32-bit */
    uint32_t len : 31;
    uint8_t is_wide_char : 1; /* 0 = 8 bits, 1 = 16 bits characters */
    /* for JS_ATOM_TYPE_SYMBOL: hash = 0, atom_type = 3,
       for JS_ATOM_TYPE_PRIVATE: hash = 1, atom_type = 3
       XXX: could change encoding to have one more bit in hash */
    uint32_t hash : 30;
    uint8_t atom_type : 2; /* != 0 if atom, JS_ATOM_TYPE_x */
    uint32_t hash_next; /* atom_index for JS_ATOM_TYPE_SYMBOL */
#ifdef DUMP_LEAKS
    struct list_head link; /* string list */
#endif
    union {
        uint8_t str8[0]; /* 8 bit strings will get an extra null terminator */
        uint16_t str16[0];
    } u;
};

typedef struct JSClosureVar {
    uint8_t is_local : 1;
    uint8_t is_arg : 1;
    uint8_t is_const : 1;
    uint8_t is_lexical : 1;
    uint8_t var_kind : 4; /* see JSVarKindEnum */
    /* 8 bits available */
    uint16_t var_idx; /* is_local = TRUE: index to a normal variable of the
                    parent function. otherwise: index to a closure
                    variable of the parent function */
    JSAtom var_name;
} JSClosureVar;

#define ARG_SCOPE_INDEX 1
#define ARG_SCOPE_END (-2)

typedef struct JSVarScope {
    int parent;  /* index into fd->scopes of the enclosing scope */
    int first;   /* index into fd->vars of the last variable in this scope */
} JSVarScope;

typedef enum {
    /* XXX: add more variable kinds here instead of using bit fields */
    JS_VAR_NORMAL,
    JS_VAR_FUNCTION_DECL, /* lexical var with function declaration */
    JS_VAR_NEW_FUNCTION_DECL, /* lexical var with async/generator
                                 function declaration */
    JS_VAR_CATCH,
    JS_VAR_FUNCTION_NAME, /* function expression name */
    JS_VAR_PRIVATE_FIELD,
    JS_VAR_PRIVATE_METHOD,
    JS_VAR_PRIVATE_GETTER,
    JS_VAR_PRIVATE_SETTER, /* must come after JS_VAR_PRIVATE_GETTER */
    JS_VAR_PRIVATE_GETTER_SETTER, /* must come after JS_VAR_PRIVATE_SETTER */
} JSVarKindEnum;

/* XXX: could use a different structure in bytecode functions to save
   memory */
typedef struct JSVarDef {
    JSAtom var_name;
    /* index into fd->scopes of this variable lexical scope */
    int scope_level;
    /* during compilation:
        - if scope_level = 0: scope in which the variable is defined
        - if scope_level != 0: index into fd->vars of the next
          variable in the same or enclosing lexical scope
       in a bytecode function:
       index into fd->vars of the next
       variable in the same or enclosing lexical scope
    */
    int scope_next;
    uint8_t is_const : 1;
    uint8_t is_lexical : 1;
    uint8_t is_captured : 1;
    uint8_t var_kind : 4; /* see JSVarKindEnum */
    /* only used during compilation: function pool index for lexical
       variables with var_kind =
       JS_VAR_FUNCTION_DECL/JS_VAR_NEW_FUNCTION_DECL or scope level of
       the definition of the 'var' variables (they have scope_level =
       0) */
    int func_pool_idx : 24; /* only used during compilation : index in
                               the constant pool for hoisted function
                               definition */
} JSVarDef;

/* for the encoding of the pc2line table */
#define PC2LINE_BASE     (-1)
#define PC2LINE_RANGE    5
#define PC2LINE_OP_FIRST 1
#define PC2LINE_DIFF_PC_MAX ((255 - PC2LINE_OP_FIRST) / PC2LINE_RANGE)

typedef enum JSFunctionKindEnum {
    JS_FUNC_NORMAL = 0,
    JS_FUNC_GENERATOR = (1 << 0),
    JS_FUNC_ASYNC = (1 << 1),
    JS_FUNC_ASYNC_GENERATOR = (JS_FUNC_GENERATOR | JS_FUNC_ASYNC),
} JSFunctionKindEnum;

typedef struct JSFunctionBytecode {
    JSGCObjectHeader header; /* must come first */
    uint8_t js_mode;
    uint8_t has_prototype : 1; /* true if a prototype field is necessary */
    uint8_t has_simple_parameter_list : 1;
    uint8_t is_derived_class_constructor : 1;
    /* true if home_object needs to be initialized */
    uint8_t need_home_object : 1;
    uint8_t func_kind : 2;
    uint8_t new_target_allowed : 1;
    uint8_t super_call_allowed : 1;
    uint8_t super_allowed : 1;
    uint8_t arguments_allowed : 1;
    uint8_t has_debug : 1;
    uint8_t backtrace_barrier : 1; /* stop backtrace on this function */
    uint8_t read_only_bytecode : 1;
    /* XXX: 4 bits available */
    uint8_t *byte_code_buf; /* (self pointer) */
    int byte_code_len;
    JSAtom func_name;
    JSVarDef *vardefs; /* arguments + local variables (arg_count + var_count) (self pointer) */
    JSClosureVar *closure_var; /* list of variables in the closure (self pointer) */
    uint16_t arg_count;
    uint16_t var_count;
    uint16_t defined_arg_count; /* for length function property */
    uint16_t stack_size; /* maximum stack size */
    JSContext *realm; /* function realm */
    JSValue *cpool; /* constant pool (self pointer) */
    int cpool_count;
    int closure_var_count;
    struct {
        /* debug info, move to separate structure to save memory? */
        JSAtom filename;
        int line_num;
        int source_len;
        int pc2line_len;
        uint8_t *pc2line_buf;
        char *source;
    } debug;
} JSFunctionBytecode;

typedef struct JSBoundFunction {
    JSValue func_obj;
    JSValue this_val;
    int argc;
    JSValue argv[0];
} JSBoundFunction;

typedef enum JSIteratorKindEnum {
    JS_ITERATOR_KIND_KEY,
    JS_ITERATOR_KIND_VALUE,
    JS_ITERATOR_KIND_KEY_AND_VALUE,
} JSIteratorKindEnum;

typedef struct JSForInIterator {
    JSValue obj;
    BOOL is_array;
    uint32_t array_length;
    uint32_t idx;
} JSForInIterator;

typedef struct JSRegExp {
    JSString *pattern;
    JSString *bytecode; /* also contains the flags */
} JSRegExp;

typedef struct JSProxyData {
    JSValue target;
    JSValue handler;
    uint8_t is_func;
    uint8_t is_revoked;
} JSProxyData;

typedef struct JSArrayBuffer {
    int byte_length; /* 0 if detached */
    uint8_t detached;
    uint8_t shared; /* if shared, the array buffer cannot be detached */
    uint8_t *data; /* NULL if detached */
    struct list_head array_list;
    void *opaque;
    JSFreeArrayBufferDataFunc *free_func;
} JSArrayBuffer;

typedef struct JSTypedArray {
    struct list_head link; /* link to arraybuffer */
    JSObject *obj; /* back pointer to the TypedArray/DataView object */
    JSObject *buffer; /* based array buffer */
    uint32_t offset; /* offset in the array buffer */
    uint32_t length; /* length in the array buffer */
} JSTypedArray;

typedef struct JSAsyncFunctionState {
    JSValue this_val; /* 'this' generator argument */
    int argc; /* number of function arguments */
    BOOL throw_flag; /* used to throw an exception in JS_CallInternal() */
    JSStackFrame frame;
} JSAsyncFunctionState;

/* XXX: could use an object instead to avoid the
   JS_TAG_ASYNC_FUNCTION tag for the GC */
typedef struct JSAsyncFunctionData {
    JSGCObjectHeader header; /* must come first */
    JSValue resolving_funcs[2];
    BOOL is_active; /* true if the async function state is valid */
    JSAsyncFunctionState func_state;
} JSAsyncFunctionData;

typedef enum {
   /* binary operators */
   JS_OVOP_ADD,
   JS_OVOP_SUB,
   JS_OVOP_MUL,
   JS_OVOP_DIV,
   JS_OVOP_MOD,
   JS_OVOP_POW,
   JS_OVOP_OR,
   JS_OVOP_AND,
   JS_OVOP_XOR,
   JS_OVOP_SHL,
   JS_OVOP_SAR,
   JS_OVOP_SHR,
   JS_OVOP_EQ,
   JS_OVOP_LESS,

   JS_OVOP_BINARY_COUNT,
   /* unary operators */
   JS_OVOP_POS = JS_OVOP_BINARY_COUNT,
   JS_OVOP_NEG,
   JS_OVOP_INC,
   JS_OVOP_DEC,
   JS_OVOP_NOT,

   JS_OVOP_COUNT,
} JSOverloadableOperatorEnum;

typedef struct {
    uint32_t operator_index;
    JSObject *ops[JS_OVOP_BINARY_COUNT]; /* self operators */
} JSBinaryOperatorDefEntry;

typedef struct {
    int count;
    JSBinaryOperatorDefEntry *tab;
} JSBinaryOperatorDef;

typedef struct {
    uint32_t operator_counter;
    BOOL is_primitive; /* OperatorSet for a primitive type */
    /* NULL if no operator is defined */
    JSObject *self_ops[JS_OVOP_COUNT]; /* self operators */
    JSBinaryOperatorDef left;
    JSBinaryOperatorDef right;
} JSOperatorSetData;

typedef struct JSReqModuleEntry {
    JSAtom module_name;
    JSModuleDef *module; /* used using resolution */
} JSReqModuleEntry;

typedef enum JSExportTypeEnum {
    JS_EXPORT_TYPE_LOCAL,
    JS_EXPORT_TYPE_INDIRECT,
} JSExportTypeEnum;

typedef struct JSExportEntry {
    union {
        struct {
            int var_idx; /* closure variable index */
            JSVarRef *var_ref; /* if != NULL, reference to the variable */
        } local; /* for local export */
        int req_module_idx; /* module for indirect export */
    } u;
    JSExportTypeEnum export_type;
    JSAtom local_name; /* '*' if export ns from. not used for local
                          export after compilation */
    JSAtom export_name; /* exported variable name */
} JSExportEntry;

typedef struct JSStarExportEntry {
    int req_module_idx; /* in req_module_entries */
} JSStarExportEntry;

typedef struct JSImportEntry {
    int var_idx; /* closure variable index */
    JSAtom import_name;
    int req_module_idx; /* in req_module_entries */
} JSImportEntry;

struct JSModuleDef {
    JSRefCountHeader header; /* must come first, 32-bit */
    JSAtom module_name;
    struct list_head link;

    JSReqModuleEntry *req_module_entries;
    int req_module_entries_count;
    int req_module_entries_size;

    JSExportEntry *export_entries;
    int export_entries_count;
    int export_entries_size;

    JSStarExportEntry *star_export_entries;
    int star_export_entries_count;
    int star_export_entries_size;

    JSImportEntry *import_entries;
    int import_entries_count;
    int import_entries_size;

    JSValue module_ns;
    JSValue func_obj; /* only used for JS modules */
    JSModuleInitFunc *init_func; /* only used for C modules */
    BOOL resolved : 8;
    BOOL func_created : 8;
    BOOL instantiated : 8;
    BOOL evaluated : 8;
    BOOL eval_mark : 8; /* temporary use during js_evaluate_module() */
    /* true if evaluation yielded an exception. It is saved in
       eval_exception */
    BOOL eval_has_exception : 8;
    JSValue eval_exception;
    JSValue meta_obj; /* for import.meta */
};

typedef struct JSJobEntry {
    struct list_head link;
    JSContext *ctx;
    JSJobFunc *job_func;
    int argc;
    JSValue argv[0];
} JSJobEntry;

typedef struct JSProperty {
    union {
        JSValue value;      /* JS_PROP_NORMAL */
        struct {            /* JS_PROP_GETSET */
            JSObject *getter; /* NULL if undefined */
            JSObject *setter; /* NULL if undefined */
        } getset;
        JSVarRef *var_ref;  /* JS_PROP_VARREF */
        struct {            /* JS_PROP_AUTOINIT */
            /* in order to use only 2 pointers, we compress the realm
               and the init function pointer */
            uintptr_t realm_and_id; /* realm and init_id (JS_AUTOINIT_ID_x)
                                       in the 2 low bits */
            void *opaque;
        } init;
    } u;
} JSProperty;

#define JS_PROP_INITIAL_SIZE 2
#define JS_PROP_INITIAL_HASH_SIZE 4 /* must be a power of two */
#define JS_ARRAY_INITIAL_SIZE 2

typedef struct JSShapeProperty {
    uint32_t hash_next : 26; /* 0 if last in list */
    uint32_t flags : 6;   /* JS_PROP_XXX */
    JSAtom atom; /* JS_ATOM_NULL = free property entry */
} JSShapeProperty;

struct JSShape {
    /* hash table of size hash_mask + 1 before the start of the
       structure (see prop_hash_end()). */
    JSGCObjectHeader header;
    /* true if the shape is inserted in the shape hash table. If not,
       JSShape.hash is not valid */
    uint8_t is_hashed;
    /* If true, the shape may have small array index properties 'n' with 0
       <= n <= 2^31-1. If false, the shape is guaranteed not to have
       small array index properties */
    uint8_t has_small_array_index;
    uint32_t hash; /* current hash value */
    uint32_t prop_hash_mask;
    int prop_size; /* allocated properties */
    int prop_count; /* include deleted properties */
    int deleted_prop_count;
    JSShape *shape_hash_next; /* in JSRuntime.shape_hash[h] list */
    JSObject *proto;
    JSShapeProperty prop[0]; /* prop_size elements */
};

struct JSObject {
    union {
        JSGCObjectHeader header;
        struct {
            int __gc_ref_count; /* corresponds to header.ref_count */
            uint8_t __gc_mark; /* corresponds to header.mark/gc_obj_type */

            uint8_t extensible : 1;
            uint8_t free_mark : 1; /* only used when freeing objects with cycles */
            uint8_t is_exotic : 1; /* TRUE if object has exotic property handlers */
            uint8_t fast_array : 1; /* TRUE if u.array is used for get/put (for JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS and typed arrays) */
            uint8_t is_constructor : 1; /* TRUE if object is a constructor function */
            uint8_t is_uncatchable_error : 1; /* if TRUE, error is not catchable */
            uint8_t tmp_mark : 1; /* used in JS_WriteObjectRec() */
            uint8_t is_HTMLDDA : 1; /* specific annex B IsHtmlDDA behavior */
            uint16_t class_id; /* see JS_CLASS_x */
        };
    };
    /* byte offsets: 16/24 */
    JSShape *shape; /* prototype and property names + flag */
    JSProperty *prop; /* array of properties */
    /* byte offsets: 24/40 */
    struct JSMapRecord *first_weak_ref; /* XXX: use a bit and an external hash table? */
    /* byte offsets: 28/48 */
    union {
        void *opaque;
        struct JSBoundFunction *bound_function; /* JS_CLASS_BOUND_FUNCTION */
        struct JSCFunctionDataRecord *c_function_data_record; /* JS_CLASS_C_FUNCTION_DATA */
        struct JSForInIterator *for_in_iterator; /* JS_CLASS_FOR_IN_ITERATOR */
        struct JSArrayBuffer *array_buffer; /* JS_CLASS_ARRAY_BUFFER, JS_CLASS_SHARED_ARRAY_BUFFER */
        struct JSTypedArray *typed_array; /* JS_CLASS_UINT8C_ARRAY..JS_CLASS_DATAVIEW */
        struct JSFloatEnv *float_env; /* JS_CLASS_FLOAT_ENV */
        struct JSOperatorSetData *operator_set; /* JS_CLASS_OPERATOR_SET */
        struct JSMapState *map_state;   /* JS_CLASS_MAP..JS_CLASS_WEAKSET */
        struct JSMapIteratorData *map_iterator_data; /* JS_CLASS_MAP_ITERATOR, JS_CLASS_SET_ITERATOR */
        struct JSArrayIteratorData *array_iterator_data; /* JS_CLASS_ARRAY_ITERATOR, JS_CLASS_STRING_ITERATOR */
        struct JSRegExpStringIteratorData *regexp_string_iterator_data; /* JS_CLASS_REGEXP_STRING_ITERATOR */
        struct JSGeneratorData *generator_data; /* JS_CLASS_GENERATOR */
        struct JSProxyData *proxy_data; /* JS_CLASS_PROXY */
        struct JSPromiseData *promise_data; /* JS_CLASS_PROMISE */
        struct JSPromiseFunctionData *promise_function_data; /* JS_CLASS_PROMISE_RESOLVE_FUNCTION, JS_CLASS_PROMISE_REJECT_FUNCTION */
        struct JSAsyncFunctionData *async_function_data; /* JS_CLASS_ASYNC_FUNCTION_RESOLVE, JS_CLASS_ASYNC_FUNCTION_REJECT */
        struct JSAsyncFromSyncIteratorData *async_from_sync_iterator_data; /* JS_CLASS_ASYNC_FROM_SYNC_ITERATOR */
        struct JSAsyncGeneratorData *async_generator_data; /* JS_CLASS_ASYNC_GENERATOR */
        struct { /* JS_CLASS_BYTECODE_FUNCTION: 12/24 bytes */
            /* also used by JS_CLASS_GENERATOR_FUNCTION, JS_CLASS_ASYNC_FUNCTION and JS_CLASS_ASYNC_GENERATOR_FUNCTION */
            struct JSFunctionBytecode *function_bytecode;
            JSVarRef **var_refs;
            JSObject *home_object; /* for 'super' access */
        } func;
        struct { /* JS_CLASS_C_FUNCTION: 12/20 bytes */
            JSContext *realm;
            JSCFunctionType c_function;
            uint8_t length;
            uint8_t cproto;
            int16_t magic;
        } cfunc;
        /* array part for fast arrays and typed arrays */
        struct { /* JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS, JS_CLASS_UINT8C_ARRAY..JS_CLASS_FLOAT64_ARRAY */
            union {
                uint32_t size;          /* JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS */
                struct JSTypedArray *typed_array; /* JS_CLASS_UINT8C_ARRAY..JS_CLASS_FLOAT64_ARRAY */
            } u1;
            union {
                JSValue *values;        /* JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS */
                void *ptr;              /* JS_CLASS_UINT8C_ARRAY..JS_CLASS_FLOAT64_ARRAY */
                int8_t *int8_ptr;       /* JS_CLASS_INT8_ARRAY */
                uint8_t *uint8_ptr;     /* JS_CLASS_UINT8_ARRAY, JS_CLASS_UINT8C_ARRAY */
                int16_t *int16_ptr;     /* JS_CLASS_INT16_ARRAY */
                uint16_t *uint16_ptr;   /* JS_CLASS_UINT16_ARRAY */
                int32_t *int32_ptr;     /* JS_CLASS_INT32_ARRAY */
                uint32_t *uint32_ptr;   /* JS_CLASS_UINT32_ARRAY */
                int64_t *int64_ptr;     /* JS_CLASS_INT64_ARRAY */
                uint64_t *uint64_ptr;   /* JS_CLASS_UINT64_ARRAY */
                float *float_ptr;       /* JS_CLASS_FLOAT32_ARRAY */
                double *double_ptr;     /* JS_CLASS_FLOAT64_ARRAY */
            } u;
            uint32_t count; /* <= 2^31-1. 0 for a detached typed array */
        } array;    /* 12/20 bytes */
        JSRegExp regexp;    /* JS_CLASS_REGEXP: 8/16 bytes */
        JSValue object_data;    /* for JS_SetObjectData(): 8/16/16 bytes */
    } u;
    /* byte sizes: 40/48/72 */
};
enum {
    __JS_ATOM_NULL = JS_ATOM_NULL,
#define DEF(name, str) JS_ATOM_ ## name,
#include "quickjs-atom.h"
#undef DEF
    JS_ATOM_END,
};
#define JS_ATOM_LAST_KEYWORD JS_ATOM_super
#define JS_ATOM_LAST_STRICT_KEYWORD JS_ATOM_yield

typedef enum OPCodeFormat {
#define FMT(f) OP_FMT_ ## f,
#define DEF(id, size, n_pop, n_push, f)
#include "quickjs-opcode.h"
#undef DEF
#undef FMT
} OPCodeFormat;

enum OPCodeEnum {
#define FMT(f)
#define DEF(id, size, n_pop, n_push, f) OP_ ## id,
#define def(id, size, n_pop, n_push, f)
#include "quickjs-opcode.h"
#undef def
#undef DEF
#undef FMT
    OP_COUNT, /* excluding temporary opcodes */
    /* temporary opcodes : overlap with the short opcodes */
    OP_TEMP_START = OP_nop + 1,
    OP___dummy = OP_TEMP_START - 1,
#define FMT(f)
#define DEF(id, size, n_pop, n_push, f)
#define def(id, size, n_pop, n_push, f) OP_ ## id,
#include "quickjs-opcode.h"
#undef def
#undef DEF
#undef FMT
    OP_TEMP_END,
};

JSValue JS_CallInternal(JSContext *ctx, JSValueConst func_obj,
                        JSValueConst this_obj, JSValueConst new_target,
                        int argc, JSValue *argv, int flags);
JSValue JS_EvalObject(JSContext *ctx, JSValueConst this_obj,
                      JSValueConst val, int flags, int scope_idx);
JSValue js_function_apply(JSContext *ctx, JSValueConst this_val,
                          int argc, JSValueConst *argv, int magic);
void js_array_finalizer(JSRuntime *rt, JSValue val);
void js_array_mark(JSRuntime *rt, JSValueConst val,
                   JS_MarkFunc *mark_func);
void js_regexp_finalizer(JSRuntime *rt, JSValue val);
void js_array_buffer_finalizer(JSRuntime *rt, JSValue val);
void js_typed_array_finalizer(JSRuntime *rt, JSValue val);
void js_typed_array_mark(JSRuntime *rt, JSValueConst val,
                         JS_MarkFunc *mark_func);
void js_array_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_array_iterator_mark(JSRuntime *rt, JSValueConst val,
                         JS_MarkFunc *mark_func);
void js_regexp_string_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_regexp_string_iterator_mark(JSRuntime *rt, JSValueConst val,
                                    JS_MarkFunc *mark_func);
void js_operator_set_finalizer(JSRuntime *rt, JSValue val);
void js_operator_set_mark(JSRuntime *rt, JSValueConst val,
                          JS_MarkFunc *mark_func);
int JS_ToUint8ClampFree(JSContext *ctx, int32_t *pres, JSValue val);
JSValue js_regexp_constructor_internal(JSContext *ctx, JSValueConst ctor,
                                       JSValue pattern, JSValue bc);

typedef enum JSStrictEqModeEnum {
    JS_EQ_STRICT,
    JS_EQ_SAME_VALUE,
    JS_EQ_SAME_VALUE_ZERO,
} JSStrictEqModeEnum;

static inline bf_t *JS_GetBigFloat(JSValueConst val)
{
    JSBigFloat *p = JS_VALUE_GET_PTR(val);
    return &p->num;
}
static inline bfdec_t *JS_GetBigDecimal(JSValueConst val)
{
    JSBigDecimal *p = JS_VALUE_GET_PTR(val);
    return &p->num;
}
static inline bf_t *JS_GetBigInt(JSValueConst val)
{
    JSBigFloat *p = JS_VALUE_GET_PTR(val);
    return &p->num;
}

BOOL js_strict_eq2(JSContext *ctx, JSValue op1, JSValue op2,
                   JSStrictEqModeEnum eq_mode);
BOOL js_strict_eq(JSContext *ctx, JSValue op1, JSValue op2);
BOOL js_same_value(JSContext *ctx, JSValueConst op1, JSValueConst op2);
BOOL js_same_value_zero(JSContext *ctx, JSValueConst op1, JSValueConst op2);
JSValue JS_ToObject(JSContext *ctx, JSValueConst val);
JSValue JS_ToObjectFree(JSContext *ctx, JSValue val);
JSProperty *add_property(JSContext *ctx,
                         JSObject *p, JSAtom prop, int prop_flags);
static void JS_FreeBigInt(JSContext *ctx, bf_t *a, bf_t *buf);
static JSValue JS_ThrowTypeErrorRevokedProxy(JSContext *ctx);
static int js_string_memcmp(const JSString *p1, const JSString *p2, int len);
void js_generator_finalizer(JSRuntime *rt, JSValue obj);
void js_generator_mark(JSRuntime *rt, JSValueConst val,
                         JS_MarkFunc *mark_func);
JSValue js_generator_function_call(JSContext *ctx, JSValueConst func_obj,
                                   JSValueConst this_obj,
                                   int argc, JSValueConst *argv,
                                   int flags);
static JSValue JS_ToNumberFree(JSContext *ctx, JSValue val);
int JS_GetOwnPropertyDesc(JSContext *ctx, JSPropertyDescriptor *desc,
                          JSObject *p, JSAtom prop);
int JS_GetOwnPropertyNoDesc(JSContext *ctx, JSObject *p, JSAtom prop);
void free_arg_list(JSContext *ctx, JSValue *tab, uint32_t len);
JSValue *build_arg_list(JSContext *ctx, uint32_t *plen,
                        JSValueConst array_arg);

size_t js_malloc_usable_size_unknown(const void *ptr);

void *js_bf_realloc(void *opaque, void *ptr, size_t size);

int js_realloc_array(JSContext *ctx, void **parray,
                     int elem_size, int *psize, int req_size);

/* resize the array and update its size if req_size > *psize */
static inline
int js_resize_array(JSContext *ctx, void **parray, int elem_size,
                                  int *psize, int req_size)
{
    if (unlikely(req_size > *psize))
        return js_realloc_array(ctx, parray, elem_size, psize, req_size);
    else
        return 0;
}

static inline
void js_dbuf_init(JSContext *ctx, DynBuf *s)
{
    dbuf_init2(s, ctx->rt, (DynBufReallocFunc *)js_realloc_rt);
}

static inline
int is_digit(int c) {
    return c >= '0' && c <= '9';
}

typedef struct JSClassShortDef {
    JSAtom class_name;
    JSClassFinalizer *finalizer;
    JSClassGCMark *gc_mark;
} JSClassShortDef;

int init_class_range(JSRuntime *rt, JSClassShortDef const *tab,
                     int start, int count);

extern JSClassShortDef const js_std_class_def[];

#if !defined(CONFIG_STACK_CHECK)
/* no stack limitation */
static inline uintptr_t js_get_stack_pointer(void)
{
    return 0;
}

static inline BOOL js_check_stack_overflow(JSRuntime *rt, size_t alloca_size)
{
    return FALSE;
}
#else
/* Note: OS and CPU dependent */
static inline uintptr_t js_get_stack_pointer(void)
{
    return (uintptr_t)__builtin_frame_address(0);
}

static inline BOOL js_check_stack_overflow(JSRuntime *rt, size_t alloca_size)
{
    uintptr_t sp;
    sp = js_get_stack_pointer() - alloca_size;
    return unlikely(sp < rt->stack_limit);
}
#endif

extern const JSMallocFunctions def_malloc_funcs;

static inline uint32_t atom_get_free(const JSAtomStruct *p)
{
    return (uintptr_t)p >> 1;
}

static inline BOOL atom_is_free(const JSAtomStruct *p)
{
    return (uintptr_t)p & 1;
}

static inline JSAtomStruct *atom_set_free(uint32_t v)
{
    return (JSAtomStruct *)(((uintptr_t)v << 1) | 1);
}

JSString *js_alloc_string_rt(JSRuntime *rt, int max_len, int is_wide_char);

JSString *js_alloc_string(JSContext *ctx, int max_len, int is_wide_char);

void JS_FreeAtomStruct(JSRuntime *rt, JSAtomStruct *p);

/* same as JS_FreeValueRT() but faster */
static inline void js_free_string(JSRuntime *rt, JSString *str)
{
    if (--str->header.ref_count <= 0) {
        if (str->atom_type) {
            JS_FreeAtomStruct(rt, str);
        } else {
#ifdef DUMP_LEAKS
            list_del(&str->link);
#endif
            js_free_rt(rt, str);
        }
    }
}

/* set the new value and free the old value after (freeing the value
   can reallocate the object data) */
static inline void set_value(JSContext *ctx, JSValue *pval, JSValue new_val)
{
    JSValue old_val;
    old_val = *pval;
    *pval = new_val;
    JS_FreeValue(ctx, old_val);
}

typedef enum JSFreeModuleEnum {
    JS_FREE_MODULE_ALL,
    JS_FREE_MODULE_NOT_RESOLVED,
    JS_FREE_MODULE_NOT_EVALUATED,
} JSFreeModuleEnum;

void js_free_modules(JSContext *ctx, JSFreeModuleEnum flag);

JSModuleDef *js_new_module_def(JSContext *ctx, JSAtom name);
void js_mark_module_def(JSRuntime *rt, JSModuleDef *m, JS_MarkFunc *mark_func);
void js_free_module_def(JSContext *ctx, JSModuleDef *m);

static inline BOOL is_strict_mode(JSContext *ctx)
{
    JSStackFrame *sf = ctx->rt->current_stack_frame;
    return (sf && (sf->js_mode & JS_MODE_STRICT));
}

static inline BOOL is_math_mode(JSContext *ctx)
{
    JSStackFrame *sf = ctx->rt->current_stack_frame;
    return (sf && (sf->js_mode & JS_MODE_MATH));
}

/* JSAtom support */

#define JS_ATOM_TAG_INT (1U << 31)
#define JS_ATOM_MAX_INT (JS_ATOM_TAG_INT - 1)
#define JS_ATOM_MAX     ((1U << 30) - 1)

/* return the max count from the hash size */
#define JS_ATOM_COUNT_RESIZE(n) ((n) * 2)

static inline BOOL __JS_AtomIsConst(JSAtom v)
{
#if defined(DUMP_LEAKS) && DUMP_LEAKS > 1
        return (int32_t)v <= 0;
#else
        return (int32_t)v < JS_ATOM_END;
#endif
}

static inline BOOL __JS_AtomIsTaggedInt(JSAtom v)
{
    return (v & JS_ATOM_TAG_INT) != 0;
}

static inline JSAtom __JS_AtomFromUInt32(uint32_t v)
{
    return v | JS_ATOM_TAG_INT;
}

static inline uint32_t __JS_AtomToUInt32(JSAtom atom)
{
    return atom & ~JS_ATOM_TAG_INT;
}

static inline int is_num(int c)
{
    return c >= '0' && c <= '9';
}

/* XXX: could use faster version ? */
static inline
uint32_t hash_string8(const uint8_t *str, size_t len, uint32_t h)
{
    size_t i;

    for(i = 0; i < len; i++)
        h = h * 263 + str[i];
    return h;
}

static inline
uint32_t hash_string16(const uint16_t *str, size_t len, uint32_t h)
{
    size_t i;

    for(i = 0; i < len; i++)
        h = h * 263 + str[i];
    return h;
}

static inline
uint32_t hash_string(const JSString *str, uint32_t h)
{
    if (str->is_wide_char)
        h = hash_string16(str->u.str16, str->len, h);
    else
        h = hash_string8(str->u.str8, str->len, h);
    return h;
}

void JS_DumpString(JSRuntime *rt, const JSString *p);

int JS_InitAtoms(JSRuntime *rt);

static inline
JSAtom JS_DupAtomRT(JSRuntime *rt, JSAtom v)
{
    JSAtomStruct *p;

    if (!__JS_AtomIsConst(v)) {
        p = rt->atom_array[v];
        p->header.ref_count++;
    }
    return v;
}

JSAtomKindEnum JS_AtomGetKind(JSContext *ctx, JSAtom v);

JSAtom js_get_atom_index(JSRuntime *rt, JSAtomStruct *p);

JSAtom __JS_NewAtom(JSRuntime *rt, JSString *str, int atom_type);

JSAtom __JS_NewAtomInit(JSRuntime *rt, const char *str, int len, int atom_type);

JSAtom __JS_FindAtom(JSRuntime *rt, const char *str, size_t len, int atom_type);

JSAtom JS_NewAtomStr(JSContext *ctx, JSString *p);

JSValue JS_NewSymbolFromAtom(JSContext *ctx, JSAtom descr,
                             int atom_type);

#define ATOM_GET_STR_BUF_SIZE 64

const char *JS_AtomGetStrRT(JSRuntime *rt, char *buf, int buf_size, JSAtom atom);

static inline
const char *JS_AtomGetStr(JSContext *ctx, char *buf, int buf_size, JSAtom atom)
{
    return JS_AtomGetStrRT(ctx->rt, buf, buf_size, atom);
}

BOOL JS_AtomIsArrayIndex(JSContext *ctx, uint32_t *pval, JSAtom atom);

JSValue JS_AtomIsNumericIndex1(JSContext *ctx, JSAtom atom);
int JS_AtomIsNumericIndex(JSContext *ctx, JSAtom atom);

BOOL JS_AtomSymbolHasDescription(JSContext *ctx, JSAtom v);

static inline BOOL JS_IsEmptyString(JSValueConst v)
{
    return JS_VALUE_GET_TAG(v) == JS_TAG_STRING && JS_VALUE_GET_STRING(v)->len == 0;
}

JSValue js_new_string8(JSContext *ctx, const uint8_t *buf, int len);
JSValue js_new_string16(JSContext *ctx, const uint16_t *buf, int len);

static inline
JSValue js_new_string_char(JSContext *ctx, uint16_t c)
{
    if (c < 0x100) {
        uint8_t ch8 = c;
        return js_new_string8(ctx, &ch8, 1);
    } else {
        uint16_t ch16 = c;
        return js_new_string16(ctx, &ch16, 1);
    }
}

JSValue js_sub_string(JSContext *ctx, JSString *p, int start, int end);

typedef struct StringBuffer {
    JSContext *ctx;
    JSString *str;
    int len;
    int size;
    int is_wide_char;
    int error_status;
} StringBuffer;

int string_buffer_init2(JSContext *ctx, StringBuffer *s, int size,
                        int is_wide);

static inline int string_buffer_init(JSContext *ctx, StringBuffer *s, int size)
{
    return string_buffer_init2(ctx, s, size, 0);
}

static inline void string_buffer_free(StringBuffer *s)
{
    js_free(s->ctx, s->str);
    s->str = NULL;
}

int string_buffer_realloc(StringBuffer *s, int new_len, int c);
int string_buffer_putc_slow(StringBuffer *s, uint32_t c);
int string_buffer_putc8(StringBuffer *s, uint32_t c);
int string_buffer_putc16(StringBuffer *s, uint32_t c);
int string_buffer_putc(StringBuffer *s, uint32_t c);

static inline int string_get(const JSString *p, int idx)
{
    return p->is_wide_char ? p->u.str16[idx] : p->u.str8[idx];
}

int string_getc(const JSString *p, int *pidx);
int string_buffer_write8(StringBuffer *s, const uint8_t *p, int len);

/* appending an ASCII string */
static inline int string_buffer_puts8(StringBuffer *s, const char *str)
{
    return string_buffer_write8(s, (const uint8_t *)str, strlen(str));
}

int string_buffer_concat(StringBuffer *s, const JSString *p,
                         uint32_t from, uint32_t to);

int string_buffer_concat_value(StringBuffer *s, JSValueConst v);
int string_buffer_concat_value_free(StringBuffer *s, JSValue v);

JSValue string_buffer_end(StringBuffer *s);

JSValue JS_ConcatString3(JSContext *ctx, const char *str1,
                         JSValue str2, const char *str3);

static int memcmp16_8(const uint16_t *src1, const uint8_t *src2, int len)
{
    int c, i;
    for(i = 0; i < len; i++) {
        c = src1[i] - src2[i];
        if (c != 0)
            return c;
    }
    return 0;
}

static int memcmp16(const uint16_t *src1, const uint16_t *src2, int len)
{
    int c, i;
    for(i = 0; i < len; i++) {
        c = src1[i] - src2[i];
        if (c != 0)
            return c;
    }
    return 0;
}

static int js_string_memcmp(const JSString *p1, const JSString *p2, int len)
{
    int res;

    if (likely(!p1->is_wide_char)) {
        if (likely(!p2->is_wide_char))
            res = memcmp(p1->u.str8, p2->u.str8, len);
        else
            res = -memcmp16_8(p2->u.str16, p1->u.str8, len);
    } else {
        if (!p2->is_wide_char)
            res = memcmp16_8(p1->u.str16, p2->u.str8, len);
        else
            res = memcmp16(p1->u.str16, p2->u.str16, len);
    }
    return res;
}

/* return < 0, 0 or > 0 */
static inline int js_string_compare(JSContext *ctx,
                                    const JSString *p1, const JSString *p2)
{
    int res, len;
    len = min_int(p1->len, p2->len);
    res = js_string_memcmp(p1, p2, len);
    if (res == 0) {
        if (p1->len == p2->len)
            res = 0;
        else if (p1->len < p2->len)
            res = -1;
        else
            res = 1;
    }
    return res;
}

JSValue JS_ConcatString(JSContext *ctx, JSValue op1, JSValue op2);

/* Shape support */

static inline
size_t get_shape_size(size_t hash_size, size_t prop_size)
{
    return hash_size * sizeof(uint32_t) + sizeof(JSShape) +
        prop_size * sizeof(JSShapeProperty);
}

static inline
JSShape *get_shape_from_alloc(void *sh_alloc, size_t hash_size)
{
    return (JSShape *)(void *)((uint32_t *)sh_alloc + hash_size);
}

static inline
uint32_t *prop_hash_end(JSShape *sh)
{
    return (uint32_t *)sh;
}

static inline
void *get_alloc_from_shape(JSShape *sh)
{
    return prop_hash_end(sh) - ((intptr_t)sh->prop_hash_mask + 1);
}

static inline
JSShapeProperty *get_shape_prop(JSShape *sh)
{
    return sh->prop;
}

static inline
int init_shape_hash(JSRuntime *rt)
{
    rt->shape_hash_bits = 4;   /* 16 shapes */
    rt->shape_hash_size = 1 << rt->shape_hash_bits;
    rt->shape_hash_count = 0;
    rt->shape_hash = js_mallocz_rt(rt, sizeof(rt->shape_hash[0]) *
                                   rt->shape_hash_size);
    if (!rt->shape_hash)
        return -1;
    return 0;
}

/* same magic hash multiplier as the Linux kernel */
static inline
uint32_t shape_hash(uint32_t h, uint32_t val)
{
    return (h + val) * 0x9e370001;
}

/* truncate the shape hash to 'hash_bits' bits */
static inline
uint32_t get_shape_hash(uint32_t h, int hash_bits)
{
    return h >> (32 - hash_bits);
}

static inline
uint32_t shape_initial_hash(JSObject *proto)
{
    uint32_t h;
    h = shape_hash(1, (uintptr_t)proto);
    if (sizeof(proto) > 4)
        h = shape_hash(h, (uint64_t)(uintptr_t)proto >> 32);
    return h;
}

static inline
void js_shape_hash_link(JSRuntime *rt, JSShape *sh)
{
    uint32_t h;
    h = get_shape_hash(sh->hash, rt->shape_hash_bits);
    sh->shape_hash_next = rt->shape_hash[h];
    rt->shape_hash[h] = sh;
    rt->shape_hash_count++;
}

static inline
void js_shape_hash_unlink(JSRuntime *rt, JSShape *sh)
{
    uint32_t h;
    JSShape **psh;

    h = get_shape_hash(sh->hash, rt->shape_hash_bits);
    psh = &rt->shape_hash[h];
    while (*psh != sh)
        psh = &(*psh)->shape_hash_next;
    *psh = sh->shape_hash_next;
    rt->shape_hash_count--;
}

JSShape *js_new_shape2(JSContext *ctx, JSObject *proto,
                       int hash_size, int prop_size);

static inline
JSShape *js_new_shape(JSContext *ctx, JSObject *proto)
{
    return js_new_shape2(ctx, proto, JS_PROP_INITIAL_HASH_SIZE,
                         JS_PROP_INITIAL_SIZE);
}

JSShape *js_clone_shape(JSContext *ctx, JSShape *sh1);

static inline
JSShape *js_dup_shape(JSShape *sh)
{
    sh->header.ref_count++;
    return sh;
}

void js_free_shape0(JSRuntime *rt, JSShape *sh);

static inline
void js_free_shape(JSRuntime *rt, JSShape *sh)
{
    if (unlikely(--sh->header.ref_count <= 0)) {
        js_free_shape0(rt, sh);
    }
}

static inline
void js_free_shape_null(JSRuntime *rt, JSShape *sh)
{
    if (sh)
        js_free_shape(rt, sh);
}

int resize_properties(JSContext *ctx, JSShape **psh,
                      JSObject *p, uint32_t count);

int compact_properties(JSContext *ctx, JSObject *p);

int add_shape_property(JSContext *ctx, JSShape **psh,
                       JSObject *p, JSAtom atom, int prop_flags);

/* find a hashed empty shape matching the prototype. Return NULL if
   not found */
static inline
JSShape *find_hashed_shape_proto(JSRuntime *rt, JSObject *proto)
{
    JSShape *sh1;
    uint32_t h, h1;

    h = shape_initial_hash(proto);
    h1 = get_shape_hash(h, rt->shape_hash_bits);
    for(sh1 = rt->shape_hash[h1]; sh1 != NULL; sh1 = sh1->shape_hash_next) {
        if (sh1->hash == h &&
            sh1->proto == proto &&
            sh1->prop_count == 0) {
            return sh1;
        }
    }
    return NULL;
}

JSShape *find_hashed_shape_prop(JSRuntime *rt, JSShape *sh,
                                JSAtom atom, int prop_flags);

#ifdef DUMP_SHAPES
void JS_DumpShapes(JSRuntime *rt);
#endif

static inline
void js_trigger_gc(JSRuntime *rt, size_t size);

static inline
void add_gc_object(JSRuntime *rt, JSGCObjectHeader *h, JSGCObjectTypeEnum type);

static inline
JSValue JS_NewObjectFromShape(JSContext *ctx, JSShape *sh, JSClassID class_id)
{
    JSObject *p;

    js_trigger_gc(ctx->rt, sizeof(JSObject));
    p = js_malloc(ctx, sizeof(JSObject));
    if (unlikely(!p))
        goto fail;
    p->class_id = class_id;
    p->extensible = TRUE;
    p->free_mark = 0;
    p->is_exotic = 0;
    p->fast_array = 0;
    p->is_constructor = 0;
    p->is_uncatchable_error = 0;
    p->tmp_mark = 0;
    p->is_HTMLDDA = 0;
    p->first_weak_ref = NULL;
    p->u.opaque = NULL;
    p->shape = sh;
    p->prop = js_malloc(ctx, sizeof(JSProperty) * sh->prop_size);
    if (unlikely(!p->prop)) {
        js_free(ctx, p);
    fail:
        js_free_shape(ctx->rt, sh);
        return JS_EXCEPTION;
    }

    switch(class_id) {
    case JS_CLASS_OBJECT:
        break;
    case JS_CLASS_ARRAY:
        {
            JSProperty *pr;
            p->is_exotic = 1;
            p->fast_array = 1;
            p->u.array.u.values = NULL;
            p->u.array.count = 0;
            p->u.array.u1.size = 0;
            /* the length property is always the first one */
            if (likely(sh == ctx->array_shape)) {
                pr = &p->prop[0];
            } else {
                /* only used for the first array */
                /* cannot fail */
                pr = add_property(ctx, p, JS_ATOM_length,
                                  JS_PROP_WRITABLE | JS_PROP_LENGTH);
            }
            pr->u.value = JS_NewInt32(ctx, 0);
        }
        break;
    case JS_CLASS_C_FUNCTION:
        p->prop[0].u.value = JS_UNDEFINED;
        break;
    case JS_CLASS_ARGUMENTS:
    case JS_CLASS_UINT8C_ARRAY:
    case JS_CLASS_INT8_ARRAY:
    case JS_CLASS_UINT8_ARRAY:
    case JS_CLASS_INT16_ARRAY:
    case JS_CLASS_UINT16_ARRAY:
    case JS_CLASS_INT32_ARRAY:
    case JS_CLASS_UINT32_ARRAY:
    case JS_CLASS_BIG_INT64_ARRAY:
    case JS_CLASS_BIG_UINT64_ARRAY:
    case JS_CLASS_FLOAT32_ARRAY:
    case JS_CLASS_FLOAT64_ARRAY:
        p->is_exotic = 1;
        p->fast_array = 1;
        p->u.array.u.ptr = NULL;
        p->u.array.count = 0;
        break;
    case JS_CLASS_DATAVIEW:
        p->u.array.u.ptr = NULL;
        p->u.array.count = 0;
        break;
    case JS_CLASS_NUMBER:
    case JS_CLASS_STRING:
    case JS_CLASS_BOOLEAN:
    case JS_CLASS_SYMBOL:
    case JS_CLASS_DATE:
    case JS_CLASS_BIG_INT:
    case JS_CLASS_BIG_FLOAT:
    case JS_CLASS_BIG_DECIMAL:
        p->u.object_data = JS_UNDEFINED;
        goto set_exotic;
    case JS_CLASS_REGEXP:
        p->u.regexp.pattern = NULL;
        p->u.regexp.bytecode = NULL;
        goto set_exotic;
    default:
    set_exotic:
        if (ctx->rt->class_array[class_id].exotic) {
            p->is_exotic = 1;
        }
        break;
    }
    p->header.ref_count = 1;
    add_gc_object(ctx->rt, &p->header, JS_GC_OBJ_TYPE_JS_OBJECT);
    return JS_MKPTR(JS_TAG_OBJECT, p);
}

static inline
JSObject *get_proto_obj(JSValueConst proto_val)
{
    if (JS_VALUE_GET_TAG(proto_val) != JS_TAG_OBJECT)
        return NULL;
    else
        return JS_VALUE_GET_OBJ(proto_val);
}

/* WARNING: proto must be an object or JS_NULL */
static inline
JSValue JS_NewObjectProtoClass2(JSContext *ctx, JSValueConst proto_val,
                                JSClassID class_id)
{
    JSShape *sh;
    JSObject *proto;

    proto = get_proto_obj(proto_val);
    sh = find_hashed_shape_proto(ctx->rt, proto);
    if (likely(sh)) {
        sh = js_dup_shape(sh);
    } else {
        sh = js_new_shape(ctx, proto);
        if (!sh)
            return JS_EXCEPTION;
    }
    return JS_NewObjectFromShape(ctx, sh, class_id);
}

int JS_SetObjectData(JSContext *ctx, JSValueConst obj, JSValue val);

void js_function_set_properties(JSContext *ctx, JSValueConst func_obj,
                                JSAtom name, int len);

static inline
BOOL js_class_has_bytecode(JSClassID class_id)
{
    return (class_id == JS_CLASS_BYTECODE_FUNCTION ||
            class_id == JS_CLASS_GENERATOR_FUNCTION ||
            class_id == JS_CLASS_ASYNC_FUNCTION ||
            class_id == JS_CLASS_ASYNC_GENERATOR_FUNCTION);
}

/* return NULL without exception if not a function or no bytecode */
static inline
JSFunctionBytecode *JS_GetFunctionBytecode(JSValueConst val)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
        return NULL;
    p = JS_VALUE_GET_OBJ(val);
    if (!js_class_has_bytecode(p->class_id))
        return NULL;
    return p->u.func.function_bytecode;
}

JSValue JS_NewCFunction3(JSContext *ctx, JSCFunction *func,
                         const char *name,
                         int length, JSCFunctionEnum cproto, int magic,
                         JSValueConst proto_val);

typedef struct JSCFunctionDataRecord {
    JSCFunctionData *func;
    uint8_t length;
    uint8_t data_len;
    uint16_t magic;
    JSValue data[0];
} JSCFunctionDataRecord;

static force_inline JSShapeProperty *find_own_property1(JSObject *p,
                                                        JSAtom atom)
{
    JSShape *sh;
    JSShapeProperty *pr, *prop;
    intptr_t h;
    sh = p->shape;
    h = (uintptr_t)atom & sh->prop_hash_mask;
    h = prop_hash_end(sh)[-h - 1];
    prop = get_shape_prop(sh);
    while (h) {
        pr = &prop[h - 1];
        if (likely(pr->atom == atom)) {
            return pr;
        }
        h = pr->hash_next;
    }
    return NULL;
}

static force_inline JSShapeProperty *find_own_property(JSProperty **ppr,
                                                       JSObject *p,
                                                       JSAtom atom)
{
    JSShape *sh;
    JSShapeProperty *pr, *prop;
    intptr_t h;
    sh = p->shape;
    h = (uintptr_t)atom & sh->prop_hash_mask;
    h = prop_hash_end(sh)[-h - 1];
    prop = get_shape_prop(sh);
    while (h) {
        pr = &prop[h - 1];
        if (likely(pr->atom == atom)) {
            *ppr = &p->prop[h - 1];
            /* the compiler should be able to assume that pr != NULL here */
            return pr;
        }
        h = pr->hash_next;
    }
    *ppr = NULL;
    return NULL;
}

/* indicate that the object may be part of a function prototype cycle */
static inline
void set_cycle_flag(JSContext *ctx, JSValueConst obj)
{
}

void free_var_ref(JSRuntime *rt, JSVarRef *var_ref);

void js_bytecode_function_finalizer(JSRuntime *rt, JSValue val);
void js_bytecode_function_mark(JSRuntime *rt, JSValueConst val,
                               JS_MarkFunc *mark_func);

/* garbage collection */

static inline
void js_trigger_gc(JSRuntime *rt, size_t size)
{
    BOOL force_gc;
#ifdef FORCE_GC_AT_MALLOC
    force_gc = TRUE;
#else
    force_gc = ((rt->malloc_state.malloc_size + size) >
                rt->malloc_gc_threshold);
#endif
    if (force_gc) {
#ifdef DUMP_GC
        printf("GC: size=%" PRIu64 "\n",
               (uint64_t)rt->malloc_state.malloc_size);
#endif
        JS_RunGC(rt);
        rt->malloc_gc_threshold = rt->malloc_state.malloc_size +
            (rt->malloc_state.malloc_size >> 1);
    }
}

static inline
void add_gc_object(JSRuntime *rt, JSGCObjectHeader *h, JSGCObjectTypeEnum type)
{
    h->mark = 0;
    h->gc_obj_type = type;
    list_add_tail(&h->link, &rt->gc_obj_list);
}

static inline
void remove_gc_object(JSGCObjectHeader *h)
{
    list_del(&h->link);
}

static inline
void dbuf_put_leb128(DynBuf *s, uint32_t v)
{
    uint32_t a;
    for(;;) {
        a = v & 0x7f;
        v >>= 7;
        if (v != 0) {
            dbuf_putc(s, a | 0x80);
        } else {
            dbuf_putc(s, a);
            break;
        }
    }
}

static inline
void dbuf_put_sleb128(DynBuf *s, int32_t v1)
{
    uint32_t v = v1;
    dbuf_put_leb128(s, (2 * v) ^ -(v >> 31));
}

static inline
int get_leb128(uint32_t *pval, const uint8_t *buf, const uint8_t *buf_end)
{
    const uint8_t *ptr = buf;
    uint32_t v, a, i;
    v = 0;
    for(i = 0; i < 5; i++) {
        if (unlikely(ptr >= buf_end))
            break;
        a = *ptr++;
        v |= (a & 0x7f) << (i * 7);
        if (!(a & 0x80)) {
            *pval = v;
            return ptr - buf;
        }
    }
    *pval = 0;
    return -1;
}

static inline
int get_sleb128(int32_t *pval, const uint8_t *buf, const uint8_t *buf_end)
{
    int ret;
    uint32_t val;
    ret = get_leb128(&val, buf, buf_end);
    if (ret < 0) {
        *pval = 0;
        return -1;
    }
    *pval = (val >> 1) ^ -(val & 1);
    return ret;
}

#define JS_BACKTRACE_FLAG_SKIP_FIRST_LEVEL (1 << 0)
/* only taken into account if filename is provided */
#define JS_BACKTRACE_FLAG_SINGLE_LEVEL     (1 << 1)

void build_backtrace(JSContext *ctx, JSValueConst error_obj,
                     const char *filename, int line_num,
                     int backtrace_flags);

JSValue JS_ThrowError2(JSContext *ctx, JSErrorEnum error_num,
                       const char *fmt, va_list ap, BOOL add_backtrace);

static inline JSValue JS_ThrowError(JSContext *ctx, JSErrorEnum error_num,
                                    const char *fmt, va_list ap)
{
    JSRuntime *rt = ctx->rt;
    JSStackFrame *sf;
    BOOL add_backtrace;

    /* the backtrace is added later if called from a bytecode function */
    sf = rt->current_stack_frame;
    add_backtrace = !rt->in_out_of_memory &&
        (!sf || (JS_GetFunctionBytecode(sf->cur_func) == NULL));
    return JS_ThrowError2(ctx, error_num, fmt, ap, add_backtrace);
}

JSValue __attribute__((format(printf, 2, 3))) JS_ThrowSyntaxError(JSContext *ctx, const char *fmt, ...);
JSValue __attribute__((format(printf, 2, 3))) JS_ThrowTypeError(JSContext *ctx, const char *fmt, ...);

static inline int __attribute__((format(printf, 3, 4))) JS_ThrowTypeErrorOrFalse(JSContext *ctx, int flags, const char *fmt, ...)
{
    va_list ap;

    if ((flags & JS_PROP_THROW) ||
        ((flags & JS_PROP_THROW_STRICT) && is_strict_mode(ctx))) {
        va_start(ap, fmt);
        JS_ThrowError(ctx, JS_TYPE_ERROR, fmt, ap);
        va_end(ap);
        return -1;
    } else {
        return FALSE;
    }
}

/* never use it directly */
static inline JSValue __attribute__((format(printf, 3, 4))) __JS_ThrowTypeErrorAtom(JSContext *ctx, JSAtom atom, const char *fmt, ...)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    return JS_ThrowTypeError(ctx, fmt,
                             JS_AtomGetStr(ctx, buf, sizeof(buf), atom));
}

/* never use it directly */
static inline JSValue __attribute__((format(printf, 3, 4))) __JS_ThrowSyntaxErrorAtom(JSContext *ctx, JSAtom atom, const char *fmt, ...)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    return JS_ThrowSyntaxError(ctx, fmt,
                               JS_AtomGetStr(ctx, buf, sizeof(buf), atom));
}

/* %s is replaced by 'atom'. The macro is used so that gcc can check
    the format string. */
#define JS_ThrowTypeErrorAtom(ctx, fmt, atom) __JS_ThrowTypeErrorAtom(ctx, atom, fmt, "")
#define JS_ThrowSyntaxErrorAtom(ctx, fmt, atom) __JS_ThrowSyntaxErrorAtom(ctx, atom, fmt, "")

static inline int JS_ThrowTypeErrorReadOnly(JSContext *ctx, int flags, JSAtom atom)
{
    if ((flags & JS_PROP_THROW) ||
        ((flags & JS_PROP_THROW_STRICT) && is_strict_mode(ctx))) {
        JS_ThrowTypeErrorAtom(ctx, "'%s' is read-only", atom);
        return -1;
    } else {
        return FALSE;
    }
}

JSValue __attribute__((format(printf, 2, 3))) JS_ThrowReferenceError(JSContext *ctx, const char *fmt, ...);
JSValue __attribute__((format(printf, 2, 3))) JS_ThrowRangeError(JSContext *ctx, const char *fmt, ...);
JSValue __attribute__((format(printf, 2, 3))) JS_ThrowInternalError(JSContext *ctx, const char *fmt, ...);
JSValue JS_ThrowOutOfMemory(JSContext *ctx);

static inline JSValue JS_ThrowStackOverflow(JSContext *ctx)
{
    return JS_ThrowInternalError(ctx, "stack overflow");
}

static inline JSValue JS_ThrowTypeErrorNotAnObject(JSContext *ctx)
{
    return JS_ThrowTypeError(ctx, "not an object");
}

static inline JSValue JS_ThrowTypeErrorNotASymbol(JSContext *ctx)
{
    return JS_ThrowTypeError(ctx, "not a symbol");
}

static inline JSValue JS_ThrowReferenceErrorNotDefined(JSContext *ctx, JSAtom name)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    return JS_ThrowReferenceError(ctx, "'%s' is not defined",
                                  JS_AtomGetStr(ctx, buf, sizeof(buf), name));
}

static inline JSValue JS_ThrowReferenceErrorUninitialized(JSContext *ctx, JSAtom name)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    return JS_ThrowReferenceError(ctx, "%s is not initialized",
                                  name == JS_ATOM_NULL ? "lexical variable" :
                                  JS_AtomGetStr(ctx, buf, sizeof(buf), name));
}

static inline JSValue JS_ThrowReferenceErrorUninitialized2(JSContext *ctx,
                                                           JSFunctionBytecode *b,
                                                           int idx, BOOL is_ref)
{
    JSAtom atom = JS_ATOM_NULL;
    if (is_ref) {
        atom = b->closure_var[idx].var_name;
    } else {
        /* not present if the function is stripped and contains no eval() */
        if (b->vardefs)
            atom = b->vardefs[b->arg_count + idx].var_name;
    }
    return JS_ThrowReferenceErrorUninitialized(ctx, atom);
}

static inline JSValue JS_ThrowTypeErrorInvalidClass(JSContext *ctx, int class_id)
{
    JSRuntime *rt = ctx->rt;
    JSAtom name;
    name = rt->class_array[class_id].class_name;
    return JS_ThrowTypeErrorAtom(ctx, "%s object expected", name);
}

__exception int __js_poll_interrupts(JSContext *ctx);

static inline __exception int js_poll_interrupts(JSContext *ctx)
{
    if (unlikely(--ctx->interrupt_counter <= 0)) {
        return __js_poll_interrupts(ctx);
    } else {
        return 0;
    }
}

int JS_SetPrototypeInternal(JSContext *ctx, JSValueConst obj,
                            JSValueConst proto_val,
                            BOOL throw_flag);

static inline
JSValue JS_GetPrototypeFree(JSContext *ctx, JSValue obj)
{
    JSValue obj1;
    obj1 = JS_GetPrototype(ctx, obj);
    JS_FreeValue(ctx, obj);
    return obj1;
}

int JS_OrdinaryIsInstanceOf(JSContext *ctx, JSValueConst val,
                            JSValueConst obj);

/* return the value associated to the autoinit property or an exception */
typedef JSValue JSAutoInitFunc(JSContext *ctx, JSObject *p, JSAtom atom, void *opaque);

extern JSAutoInitFunc *js_autoinit_func_table[];

static inline
uint32_t js_string_obj_get_length(JSContext *ctx, JSValueConst obj)
{
    JSObject *p;
    JSString *p1;
    uint32_t len = 0;

    /* This is a class exotic method: obj class_id is JS_CLASS_STRING */
    p = JS_VALUE_GET_OBJ(obj);
    if (JS_VALUE_GET_TAG(p->u.object_data) == JS_TAG_STRING) {
        p1 = JS_VALUE_GET_STRING(p->u.object_data);
        len = p1->len;
    }
    return len;
}

static inline void js_free_prop_enum(JSContext *ctx, JSPropertyEnum *tab, uint32_t len)
{
    uint32_t i;
    if (tab) {
        for(i = 0; i < len; i++)
            JS_FreeAtom(ctx, tab[i].atom);
        js_free(ctx, tab);
    }
}

int __exception JS_GetOwnPropertyNamesInternal(JSContext *ctx,
                                               JSPropertyEnum **ptab,
                                               uint32_t *plen,
                                               JSObject *p, int flags);

/* val must be a symbol */
static inline
JSAtom js_symbol_to_atom(JSContext *ctx, JSValue val)
{
    JSAtomStruct *p = JS_VALUE_GET_PTR(val);
    return js_get_atom_index(ctx->rt, p);
}

JSAtom JS_ValueToAtom(JSContext *ctx, JSValueConst val);

int JS_TryGetPropertyInt64(JSContext *ctx, JSValueConst obj, int64_t idx, JSValue *pval);

JSValue JS_GetPropertyInt64(JSContext *ctx, JSValueConst obj, int64_t idx);

__exception int convert_fast_array_to_array(JSContext *ctx, JSObject *p);

int delete_property(JSContext *ctx, JSObject *p, JSAtom atom);

int set_array_length(JSContext *ctx, JSObject *p, JSValue val, int flags);
int add_fast_array_element(JSContext *ctx, JSObject *p, JSValue val, int flags);

static inline void js_free_desc(JSContext *ctx, JSPropertyDescriptor *desc)
{
    JS_FreeValue(ctx, desc->getter);
    JS_FreeValue(ctx, desc->setter);
    JS_FreeValue(ctx, desc->value);
}

int JS_SetPropertyGeneric(JSContext *ctx,
                          JSValueConst obj, JSAtom prop,
                          JSValue val, JSValueConst this_obj,
                          int flags);

BOOL check_define_prop_flags(int prop_flags, int flags);

int js_update_property_flags(JSContext *ctx, JSObject *p,
                             JSShapeProperty **pprs, int flags);

int JS_DefineAutoInitProperty(JSContext *ctx, JSValueConst this_obj,
                              JSAtom prop, JSAutoInitIDEnum id,
                              void *opaque, int flags);

#define DEFINE_GLOBAL_LEX_VAR (1 << 7)
#define DEFINE_GLOBAL_FUNC_VAR (1 << 6)

int JS_DeletePropertyInt64(JSContext *ctx, JSValueConst obj, int64_t idx, int flags);

BOOL JS_IsCFunction(JSContext *ctx, JSValueConst val, JSCFunction *func, int magic);

#define HINT_STRING  0
#define HINT_NUMBER  1
#define HINT_NONE    2
/* don't try Symbol.toPrimitive */
#define HINT_FORCE_ORDINARY (1 << 4)

JSValue JS_ToPrimitiveFree(JSContext *ctx, JSValue val, int hint);

static inline JSValue JS_ToPrimitive(JSContext *ctx, JSValueConst val, int hint)
{
    return JS_ToPrimitiveFree(ctx, JS_DupValue(ctx, val), hint);
}

static inline
int skip_spaces(const char *pc)
{
    const uint8_t *p, *p_next, *p_start;
    uint32_t c;

    p = p_start = (const uint8_t *)pc;
    for (;;) {
        c = *p;
        if (c < 128) {
            if (!((c >= 0x09 && c <= 0x0d) || (c == 0x20)))
                break;
            p++;
        } else {
            c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p_next);
            if (!lre_is_space(c))
                break;
            p = p_next;
        }
    }
    return p - p_start;
}

static inline int to_digit(int c)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    else if (c >= 'A' && c <= 'Z')
        return c - 'A' + 10;
    else if (c >= 'a' && c <= 'z')
        return c - 'a' + 10;
    else
        return 36;
}

#define ATOD_INT_ONLY        (1 << 0)
/* accept Oo and Ob prefixes in addition to 0x prefix if radix = 0 */
#define ATOD_ACCEPT_BIN_OCT  (1 << 2)
/* accept O prefix as octal if radix == 0 and properly formed (Annex B) */
#define ATOD_ACCEPT_LEGACY_OCTAL  (1 << 4)
/* accept _ between digits as a digit separator */
#define ATOD_ACCEPT_UNDERSCORES  (1 << 5)
/* allow a suffix to override the type */
#define ATOD_ACCEPT_SUFFIX    (1 << 6)
/* default type */
#define ATOD_TYPE_MASK        (3 << 7)
#define ATOD_TYPE_FLOAT64     (0 << 7)
#define ATOD_TYPE_BIG_INT     (1 << 7)
#define ATOD_TYPE_BIG_FLOAT   (2 << 7)
#define ATOD_TYPE_BIG_DECIMAL (3 << 7)
/* assume bigint mode: floats are parsed as integers if no decimal
   point nor exponent */
#define ATOD_MODE_BIGINT      (1 << 9)
/* accept -0x1 */
#define ATOD_ACCEPT_PREFIX_AFTER_SIGN (1 << 10)

JSValue js_atof2(JSContext *ctx, const char *str, const char **pp,
                 int radix, int flags, slimb_t *pexponent);

static inline
JSValue js_atof(JSContext *ctx, const char *str, const char **pp,
                int radix, int flags)
{
    return js_atof2(ctx, str, pp, radix, flags, NULL);
}

typedef enum JSToNumberHintEnum {
    TON_FLAG_NUMBER,
    TON_FLAG_NUMERIC,
} JSToNumberHintEnum;

JSValue JS_ToNumberHintFree(JSContext *ctx, JSValue val,
                            JSToNumberHintEnum flag);

static inline JSValue JS_ToNumberFree(JSContext *ctx, JSValue val)
{
    return JS_ToNumberHintFree(ctx, val, TON_FLAG_NUMBER);
}

static inline JSValue JS_ToNumericFree(JSContext *ctx, JSValue val)
{
    return JS_ToNumberHintFree(ctx, val, TON_FLAG_NUMERIC);
}

static inline JSValue JS_ToNumeric(JSContext *ctx, JSValueConst val)
{
    return JS_ToNumericFree(ctx, JS_DupValue(ctx, val));
}

__exception
int __JS_ToFloat64Free(JSContext *ctx, double *pres, JSValue val);

static inline
int JS_ToFloat64Free(JSContext *ctx, double *pres, JSValue val)
{
    uint32_t tag;

    tag = JS_VALUE_GET_TAG(val);
    if (tag <= JS_TAG_NULL) {
        *pres = JS_VALUE_GET_INT(val);
        return 0;
    } else if (JS_TAG_IS_FLOAT64(tag)) {
        *pres = JS_VALUE_GET_FLOAT64(val);
        return 0;
    } else {
        return __JS_ToFloat64Free(ctx, pres, val);
    }
}

static inline
JSValue JS_ToNumber(JSContext *ctx, JSValueConst val)
{
    return JS_ToNumberFree(ctx, JS_DupValue(ctx, val));
}

JSValue JS_ToIntegerFree(JSContext *ctx, JSValue val);

int JS_ToInt32SatFree(JSContext *ctx, int *pres, JSValue val);

static inline
int JS_ToInt32Sat(JSContext *ctx, int *pres, JSValueConst val)
{
    return JS_ToInt32SatFree(ctx, pres, JS_DupValue(ctx, val));
}

static inline
int JS_ToInt32Clamp(JSContext *ctx, int *pres, JSValueConst val,
                    int min, int max, int min_offset)
{
    int res = JS_ToInt32SatFree(ctx, pres, JS_DupValue(ctx, val));
    if (res == 0) {
        if (*pres < min) {
            *pres += min_offset;
            if (*pres < min)
                *pres = min;
        } else {
            if (*pres > max)
                *pres = max;
        }
    }
    return res;
}

int JS_ToInt64SatFree(JSContext *ctx, int64_t *pres, JSValue val);

static inline
int JS_ToInt64Sat(JSContext *ctx, int64_t *pres, JSValueConst val)
{
    return JS_ToInt64SatFree(ctx, pres, JS_DupValue(ctx, val));
}

static inline
int JS_ToInt64Clamp(JSContext *ctx, int64_t *pres, JSValueConst val,
                    int64_t min, int64_t max, int64_t neg_offset)
{
    int res = JS_ToInt64SatFree(ctx, pres, JS_DupValue(ctx, val));
    if (res == 0) {
        if (*pres < 0)
            *pres += neg_offset;
        if (*pres < min)
            *pres = min;
        else if (*pres > max)
            *pres = max;
    }
    return res;
}

__exception int JS_ToArrayLengthFree(JSContext *ctx, uint32_t *plen,
                                     JSValue val, BOOL is_array_ctor);

#define MAX_SAFE_INTEGER (((int64_t)1 << 53) - 1)

static inline
BOOL is_safe_integer(double d)
{
    return isfinite(d) && floor(d) == d &&
        fabs(d) <= (double)MAX_SAFE_INTEGER;
}

/* convert a value to a length between 0 and MAX_SAFE_INTEGER.
   return -1 for exception */
static inline __exception
int JS_ToLengthFree(JSContext *ctx, int64_t *plen, JSValue val)
{
    int res = JS_ToInt64Clamp(ctx, plen, val, 0, MAX_SAFE_INTEGER, 0);
    JS_FreeValue(ctx, val);
    return res;
}

/* Note: can return an exception */
static inline
int JS_NumberIsInteger(JSContext *ctx, JSValueConst val)
{
    double d;
    if (!JS_IsNumber(val))
        return FALSE;
    if (unlikely(JS_ToFloat64(ctx, &d, val)))
        return -1;
    return isfinite(d) && floor(d) == d;
}

char *i64toa(char *buf_end, int64_t n, unsigned int base);

void js_ecvt1(double d, int n_digits, int *decpt, int *sign, char *buf,
              int rounding_mode, char *buf1, int buf1_size);
int js_ecvt(double d, int n_digits, int *decpt, int *sign, char *buf,
            BOOL is_fixed);
int js_fcvt1(char *buf, int buf_size, double d, int n_digits,
             int rounding_mode);

/* radix != 10 is only supported with flags = JS_DTOA_VAR_FORMAT */
/* use as many digits as necessary */
#define JS_DTOA_VAR_FORMAT   (0 << 0)
/* use n_digits significant digits (1 <= n_digits <= 101) */
#define JS_DTOA_FIXED_FORMAT (1 << 0)
/* force fractional format: [-]dd.dd with n_digits fractional digits */
#define JS_DTOA_FRAC_FORMAT  (2 << 0)
/* force exponential notation either in fixed or variable format */
#define JS_DTOA_FORCE_EXP    (1 << 2)

JSValue js_dtoa(JSContext *ctx,
                double d, int radix, int n_digits, int flags);

static inline JSValue JS_ToStringFree(JSContext *ctx, JSValue val)
{
    JSValue ret;
    ret = JS_ToString(ctx, val);
    JS_FreeValue(ctx, val);
    return ret;
}

JSValue JS_ToLocaleStringFree(JSContext *ctx, JSValue val);

static inline
JSValue JS_ToStringCheckObject(JSContext *ctx, JSValueConst val)
{
    uint32_t tag = JS_VALUE_GET_TAG(val);
    if (tag == JS_TAG_NULL || tag == JS_TAG_UNDEFINED)
        return JS_ThrowTypeError(ctx, "null or undefined are forbidden");
    return JS_ToString(ctx, val);
}

JSValue JS_ToQuotedString(JSContext *ctx, JSValueConst val1);

static inline
double js_pow(double a, double b)
{
    if (unlikely(!isfinite(b)) && fabs(a) == 1) {
        /* not compatible with IEEE 754 */
        return JS_FLOAT64_NAN;
    } else {
        return pow(a, b);
    }
}

JSValue JS_NewBigInt64_1(JSContext *ctx, int64_t v);

bf_t *JS_ToBigFloat(JSContext *ctx, bf_t *buf, JSValueConst val);

JSValue JS_StringToBigInt(JSContext *ctx, JSValue val);

static inline
JSValue JS_StringToBigIntErr(JSContext *ctx, JSValue val)
{
    val = JS_StringToBigInt(ctx, val);
    if (JS_VALUE_IS_NAN(val))
        return JS_ThrowSyntaxError(ctx, "invalid bigint literal");
    return val;
}

/* if the returned bigfloat is allocated it is equal to
   'buf'. Otherwise it is a pointer to the bigfloat in 'val'. */
bf_t *JS_ToBigIntFree(JSContext *ctx, bf_t *buf, JSValue val);

JSValue JS_ToBigIntValueFree(JSContext *ctx, JSValue val);

/* free the bf_t allocated by JS_ToBigInt */
static inline
void JS_FreeBigInt(JSContext *ctx, bf_t *a, bf_t *buf)
{
    if (a == buf) {
        bf_delete(a);
    } else {
        JSBigFloat *p = (JSBigFloat *)((uint8_t *)a -
                                       offsetof(JSBigFloat, num));
        JS_FreeValue(ctx, JS_MKPTR(JS_TAG_BIG_FLOAT, p));
    }
}

static inline
JSValue JS_NewBigFloat(JSContext *ctx)
{
    JSBigFloat *p;
    p = js_malloc(ctx, sizeof(*p));
    if (!p)
        return JS_EXCEPTION;
    p->header.ref_count = 1;
    bf_init(ctx->bf_ctx, &p->num);
    return JS_MKPTR(JS_TAG_BIG_FLOAT, p);
}

static inline
JSValue JS_NewBigInt(JSContext *ctx)
{
    JSBigFloat *p;
    p = js_malloc(ctx, sizeof(*p));
    if (!p)
        return JS_EXCEPTION;
    p->header.ref_count = 1;
    bf_init(ctx->bf_ctx, &p->num);
    return JS_MKPTR(JS_TAG_BIG_INT, p);
}

JSValue JS_CompactBigInt1(JSContext *ctx, JSValue val,
                          BOOL convert_to_safe_integer);

/* Convert the big int to a safe integer if in math mode. normalize
   the zero representation. Could also be used to convert the bigint
   to a short bigint value. The reference count of the value must be
   1. Cannot fail */
static inline
JSValue JS_CompactBigInt(JSContext *ctx, JSValue val)
{
    return JS_CompactBigInt1(ctx, val, is_math_mode(ctx));
}

extern const char js_overloadable_operator_names[JS_OVOP_COUNT][4];

int get_ovop_from_opcode(OPCodeEnum op);

JSValue throw_bf_exception(JSContext *ctx, int status);

int js_compare_bigfloat(JSContext *ctx, OPCodeEnum op,
                        JSValue op1, JSValue op2);

static inline
JSValue js_throw_type_error(JSContext *ctx, JSValueConst this_val,
                            int argc, JSValueConst *argv)
{
    return JS_ThrowTypeError(ctx, "invalid property access");
}

#define GLOBAL_VAR_OFFSET 0x40000000
#define ARGUMENT_VAR_OFFSET 0x20000000

JSValue JS_GetIterator2(JSContext *ctx, JSValueConst obj,
                        JSValueConst method);
JSValue JS_GetIteratorAsync(JSContext *ctx, JSValueConst obj, BOOL is_async);
JSValue JS_GetIterator(JSContext *ctx, JSValueConst obj);
JSValue JS_IteratorNext2(JSContext *ctx, JSValueConst enum_obj,
                         JSValueConst method,
                         int argc, JSValueConst *argv, int *pdone);
JSValue JS_IteratorNext(JSContext *ctx, JSValueConst enum_obj,
                        JSValueConst method,
                        int argc, JSValueConst *argv, BOOL *pdone);
int JS_IteratorClose(JSContext *ctx, JSValueConst enum_obj,
                     BOOL is_exception_pending);

__exception int js_for_of_start(JSContext *ctx, JSValue *sp, BOOL is_async);

JSValue JS_IteratorGetCompleteValue(JSContext *ctx, JSValueConst obj,
                                    BOOL *pdone);

JSValue js_create_iterator_result(JSContext *ctx,
                                  JSValue val,
                                  BOOL done);

JSValue js_create_array_iterator(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int magic);

static inline
BOOL js_is_fast_array(JSContext *ctx, JSValueConst obj)
{
    /* Try and handle fast arrays explicitly */
    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(obj);
        if (p->class_id == JS_CLASS_ARRAY && p->fast_array) {
            return TRUE;
        }
    }
    return FALSE;
}

/* Access an Array's internal JSValue array if available */
static inline
BOOL js_get_fast_array(JSContext *ctx, JSValueConst obj,
                       JSValue **arrpp, uint32_t *countp)
{
    /* Try and handle fast arrays explicitly */
    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(obj);
        if (p->class_id == JS_CLASS_ARRAY && p->fast_array) {
            *countp = p->u.array.count;
            *arrpp = p->u.array.u.values;
            return TRUE;
        }
    }
    return FALSE;
}

__exception int JS_CopyDataProperties(JSContext *ctx,
                                      JSValueConst target,
                                      JSValueConst source,
                                      JSValueConst excluded,
                                      BOOL setprop);

/* only valid inside C functions */
static inline JSValueConst JS_GetActiveFunction(JSContext *ctx)
{
    return ctx->rt->current_stack_frame->cur_func;
}

extern const uint16_t func_kind_to_class_id[];

JSValue js_closure(JSContext *ctx, JSValue bfunc,
                   JSVarRef **cur_var_refs,
                   JSStackFrame *sf);

#define JS_DEFINE_CLASS_HAS_HERITAGE     (1 << 0)

void close_var_refs(JSRuntime *rt, JSStackFrame *sf);

#define JS_CALL_FLAG_COPY_ARGV   (1 << 1)
#define JS_CALL_FLAG_GENERATOR   (1 << 2)

/* argument of OP_special_object */
typedef enum {
    OP_SPECIAL_OBJECT_ARGUMENTS,
    OP_SPECIAL_OBJECT_MAPPED_ARGUMENTS,
    OP_SPECIAL_OBJECT_THIS_FUNC,
    OP_SPECIAL_OBJECT_NEW_TARGET,
    OP_SPECIAL_OBJECT_HOME_OBJECT,
    OP_SPECIAL_OBJECT_VAR_OBJECT,
    OP_SPECIAL_OBJECT_IMPORT_META,
} OPSpecialObjectEnum;

#define FUNC_RET_AWAIT      0
#define FUNC_RET_YIELD      1
#define FUNC_RET_YIELD_STAR 2

#define OP_DEFINE_METHOD_METHOD 0
#define OP_DEFINE_METHOD_GETTER 1
#define OP_DEFINE_METHOD_SETTER 2
#define OP_DEFINE_METHOD_ENUMERABLE 4

#define JS_THROW_VAR_RO             0
#define JS_THROW_VAR_REDECL         1
#define JS_THROW_VAR_UNINITIALIZED  2
#define JS_THROW_ERROR_DELETE_SUPER   3
#define JS_THROW_ERROR_ITERATOR_THROW 4

static inline
JSValue JS_CallFree(JSContext *ctx, JSValue func_obj, JSValueConst this_obj,
                    int argc, JSValueConst *argv)
{
    JSValue res = JS_CallInternal(ctx, func_obj, this_obj, JS_UNDEFINED,
                                  argc, (JSValue *)argv, JS_CALL_FLAG_COPY_ARGV);
    JS_FreeValue(ctx, func_obj);
    return res;
}

JSContext *JS_GetFunctionRealm(JSContext *ctx, JSValueConst func_obj);

static inline
JSValue js_create_from_ctor(JSContext *ctx, JSValueConst ctor, int class_id)
{
    JSValue proto, obj;
    JSContext *realm;

    if (JS_IsUndefined(ctor)) {
        proto = JS_DupValue(ctx, ctx->class_proto[class_id]);
    } else {
        proto = JS_GetProperty(ctx, ctor, JS_ATOM_prototype);
        if (JS_IsException(proto))
            return proto;
        if (!JS_IsObject(proto)) {
            JS_FreeValue(ctx, proto);
            realm = JS_GetFunctionRealm(ctx, ctor);
            if (!realm)
                return JS_EXCEPTION;
            proto = JS_DupValue(ctx, realm->class_proto[class_id]);
        }
    }
    obj = JS_NewObjectProtoClass(ctx, proto, class_id);
    JS_FreeValue(ctx, proto);
    return obj;
}

static inline
JSValue JS_InvokeFree(JSContext *ctx, JSValue this_val, JSAtom atom,
                      int argc, JSValueConst *argv)
{
    JSValue res = JS_Invoke(ctx, this_val, atom, argc, argv);
    JS_FreeValue(ctx, this_val);
    return res;
}

__exception int async_func_init(JSContext *ctx, JSAsyncFunctionState *s,
                                JSValueConst func_obj, JSValueConst this_obj,
                                int argc, JSValueConst *argv);

void async_func_mark(JSRuntime *rt, JSAsyncFunctionState *s,
                     JS_MarkFunc *mark_func);

void async_func_free(JSRuntime *rt, JSAsyncFunctionState *s);

JSValue async_func_resume(JSContext *ctx, JSAsyncFunctionState *s);

/* XXX: use enum */
#define GEN_MAGIC_NEXT   0
#define GEN_MAGIC_RETURN 1
#define GEN_MAGIC_THROW  2

/* JS parser */

enum {
    TOK_NUMBER = -128,
    TOK_STRING,
    TOK_TEMPLATE,
    TOK_IDENT,
    TOK_REGEXP,
    /* warning: order matters (see js_parse_assign_expr) */
    TOK_MUL_ASSIGN,
    TOK_DIV_ASSIGN,
    TOK_MOD_ASSIGN,
    TOK_PLUS_ASSIGN,
    TOK_MINUS_ASSIGN,
    TOK_SHL_ASSIGN,
    TOK_SAR_ASSIGN,
    TOK_SHR_ASSIGN,
    TOK_AND_ASSIGN,
    TOK_XOR_ASSIGN,
    TOK_OR_ASSIGN,
    TOK_MATH_POW_ASSIGN,
    TOK_POW_ASSIGN,
    TOK_LAND_ASSIGN,
    TOK_LOR_ASSIGN,
    TOK_DOUBLE_QUESTION_MARK_ASSIGN,
    TOK_DEC,
    TOK_INC,
    TOK_SHL,
    TOK_SAR,
    TOK_SHR,
    TOK_LT,
    TOK_LTE,
    TOK_GT,
    TOK_GTE,
    TOK_EQ,
    TOK_STRICT_EQ,
    TOK_NEQ,
    TOK_STRICT_NEQ,
    TOK_LAND,
    TOK_LOR,
    TOK_MATH_POW,
    TOK_POW,
    TOK_ARROW,
    TOK_ELLIPSIS,
    TOK_DOUBLE_QUESTION_MARK,
    TOK_QUESTION_MARK_DOT,
    TOK_ERROR,
    TOK_PRIVATE_NAME,
    TOK_EOF,
    /* keywords: WARNING: same order as atoms */
    TOK_NULL, /* must be first */
    TOK_FALSE,
    TOK_TRUE,
    TOK_IF,
    TOK_ELSE,
    TOK_RETURN,
    TOK_VAR,
    TOK_THIS,
    TOK_DELETE,
    TOK_VOID,
    TOK_TYPEOF,
    TOK_NEW,
    TOK_IN,
    TOK_INSTANCEOF,
    TOK_DO,
    TOK_WHILE,
    TOK_FOR,
    TOK_BREAK,
    TOK_CONTINUE,
    TOK_SWITCH,
    TOK_CASE,
    TOK_DEFAULT,
    TOK_THROW,
    TOK_TRY,
    TOK_CATCH,
    TOK_FINALLY,
    TOK_FUNCTION,
    TOK_DEBUGGER,
    TOK_WITH,
    /* FutureReservedWord */
    TOK_CLASS,
    TOK_CONST,
    TOK_ENUM,
    TOK_EXPORT,
    TOK_EXTENDS,
    TOK_IMPORT,
    TOK_SUPER,
    /* FutureReservedWords when parsing strict mode code */
    TOK_IMPLEMENTS,
    TOK_INTERFACE,
    TOK_LET,
    TOK_PACKAGE,
    TOK_PRIVATE,
    TOK_PROTECTED,
    TOK_PUBLIC,
    TOK_STATIC,
    TOK_YIELD,
    TOK_AWAIT, /* must be last */
    TOK_OF,     /* only used for js_parse_skip_parens_token() */
};

#define TOK_FIRST_KEYWORD   TOK_NULL
#define TOK_LAST_KEYWORD    TOK_AWAIT

/* unicode code points */
#define CP_NBSP 0x00a0
#define CP_BOM  0xfeff

#define CP_LS   0x2028
#define CP_PS   0x2029

typedef struct BlockEnv {
    struct BlockEnv *prev;
    JSAtom label_name; /* JS_ATOM_NULL if none */
    int label_break; /* -1 if none */
    int label_cont; /* -1 if none */
    int drop_count; /* number of stack elements to drop */
    int label_finally; /* -1 if none */
    int scope_level;
    int has_iterator;
} BlockEnv;

typedef struct JSGlobalVar {
    int cpool_idx; /* if >= 0, index in the constant pool for hoisted
                      function defintion*/
    uint8_t force_init : 1; /* force initialization to undefined */
    uint8_t is_lexical : 1; /* global let/const definition */
    uint8_t is_const   : 1; /* const definition */
    int scope_level;    /* scope of definition */
    JSAtom var_name;  /* variable name */
} JSGlobalVar;

typedef struct RelocEntry {
    struct RelocEntry *next;
    uint32_t addr; /* address to patch */
    int size;   /* address size: 1, 2 or 4 bytes */
} RelocEntry;

typedef struct JumpSlot {
    int op;
    int size;
    int pos;
    int label;
} JumpSlot;

typedef struct LabelSlot {
    int ref_count;
    int pos;    /* phase 1 address, -1 means not resolved yet */
    int pos2;   /* phase 2 address, -1 means not resolved yet */
    int addr;   /* phase 3 address, -1 means not resolved yet */
    RelocEntry *first_reloc;
} LabelSlot;

typedef struct LineNumberSlot {
    uint32_t pc;
    int line_num;
} LineNumberSlot;

typedef enum JSParseFunctionEnum {
    JS_PARSE_FUNC_STATEMENT,
    JS_PARSE_FUNC_VAR,
    JS_PARSE_FUNC_EXPR,
    JS_PARSE_FUNC_ARROW,
    JS_PARSE_FUNC_GETTER,
    JS_PARSE_FUNC_SETTER,
    JS_PARSE_FUNC_METHOD,
    JS_PARSE_FUNC_CLASS_CONSTRUCTOR,
    JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR,
} JSParseFunctionEnum;

typedef enum JSParseExportEnum {
    JS_PARSE_EXPORT_NONE,
    JS_PARSE_EXPORT_NAMED,
    JS_PARSE_EXPORT_DEFAULT,
} JSParseExportEnum;

typedef struct JSFunctionDef {
    JSContext *ctx;
    struct JSFunctionDef *parent;
    int parent_cpool_idx; /* index in the constant pool of the parent
                             or -1 if none */
    int parent_scope_level; /* scope level in parent at point of definition */
    struct list_head child_list; /* list of JSFunctionDef.link */
    struct list_head link;

    BOOL is_eval; /* TRUE if eval code */
    int eval_type; /* only valid if is_eval = TRUE */
    BOOL is_global_var; /* TRUE if variables are not defined locally:
                           eval global, eval module or non strict eval */
    BOOL is_func_expr; /* TRUE if function expression */
    BOOL has_home_object; /* TRUE if the home object is available */
    BOOL has_prototype; /* true if a prototype field is necessary */
    BOOL has_simple_parameter_list;
    BOOL has_parameter_expressions; /* if true, an argument scope is created */
    BOOL has_use_strict; /* to reject directive in special cases */
    BOOL has_eval_call; /* true if the function contains a call to eval() */
    BOOL has_arguments_binding; /* true if the 'arguments' binding is
                                   available in the function */
    BOOL has_this_binding; /* true if the 'this' and new.target binding are
                              available in the function */
    BOOL new_target_allowed; /* true if the 'new.target' does not
                                throw a syntax error */
    BOOL super_call_allowed; /* true if super() is allowed */
    BOOL super_allowed; /* true if super. or super[] is allowed */
    BOOL arguments_allowed; /* true if the 'arguments' identifier is allowed */
    BOOL is_derived_class_constructor;
    BOOL in_function_body;
    BOOL backtrace_barrier;
    JSFunctionKindEnum func_kind : 8;
    JSParseFunctionEnum func_type : 8;
    uint8_t js_mode; /* bitmap of JS_MODE_x */
    JSAtom func_name; /* JS_ATOM_NULL if no name */

    JSVarDef *vars;
    int var_size; /* allocated size for vars[] */
    int var_count;
    JSVarDef *args;
    int arg_size; /* allocated size for args[] */
    int arg_count; /* number of arguments */
    int defined_arg_count;
    int var_object_idx; /* -1 if none */
    int arg_var_object_idx; /* -1 if none (var object for the argument scope) */
    int arguments_var_idx; /* -1 if none */
    int arguments_arg_idx; /* argument variable definition in argument scope,
                              -1 if none */
    int func_var_idx; /* variable containing the current function (-1
                         if none, only used if is_func_expr is true) */
    int eval_ret_idx; /* variable containing the return value of the eval, -1 if none */
    int this_var_idx; /* variable containg the 'this' value, -1 if none */
    int new_target_var_idx; /* variable containg the 'new.target' value, -1 if none */
    int this_active_func_var_idx; /* variable containg the 'this.active_func' value, -1 if none */
    int home_object_var_idx;
    BOOL need_home_object;

    int scope_level;    /* index into fd->scopes if the current lexical scope */
    int scope_first;    /* index into vd->vars of first lexically scoped variable */
    int scope_size;     /* allocated size of fd->scopes array */
    int scope_count;    /* number of entries used in the fd->scopes array */
    JSVarScope *scopes;
    JSVarScope def_scope_array[4];
    int body_scope; /* scope of the body of the function or eval */

    int global_var_count;
    int global_var_size;
    JSGlobalVar *global_vars;

    DynBuf byte_code;
    int last_opcode_pos; /* -1 if no last opcode */
    int last_opcode_line_num;
    BOOL use_short_opcodes; /* true if short opcodes are used in byte_code */

    LabelSlot *label_slots;
    int label_size; /* allocated size for label_slots[] */
    int label_count;
    BlockEnv *top_break; /* break/continue label stack */

    /* constant pool (strings, functions, numbers) */
    JSValue *cpool;
    int cpool_count;
    int cpool_size;

    /* list of variables in the closure */
    int closure_var_count;
    int closure_var_size;
    JSClosureVar *closure_var;

    JumpSlot *jump_slots;
    int jump_size;
    int jump_count;

    LineNumberSlot *line_number_slots;
    int line_number_size;
    int line_number_count;
    int line_number_last;
    int line_number_last_pc;

    /* pc2line table */
    JSAtom filename;
    int line_num;
    DynBuf pc2line;

    char *source;  /* raw source, utf-8 encoded */
    int source_len;

    JSModuleDef *module; /* != NULL when parsing a module */
} JSFunctionDef;

typedef struct JSToken {
    int val;
    int line_num;   /* line number of token start */
    const uint8_t *ptr;
    union {
        struct {
            JSValue str;
            int sep;
        } str;
        struct {
            JSValue val;
            slimb_t exponent; /* may be != 0 only if val is a float */
        } num;
        struct {
            JSAtom atom;
            BOOL has_escape;
            BOOL is_reserved;
        } ident;
        struct {
            JSValue body;
            JSValue flags;
        } regexp;
    } u;
} JSToken;

typedef struct JSParseState {
    JSContext *ctx;
    int last_line_num;  /* line number of last token */
    int line_num;       /* line number of current offset */
    const char *filename;
    JSToken token;
    BOOL got_lf; /* true if got line feed before the current token */
    const uint8_t *last_ptr;
    const uint8_t *buf_ptr;
    const uint8_t *buf_end;

    /* current function code */
    JSFunctionDef *cur_func;
    BOOL is_module; /* parsing a module */
    BOOL allow_html_comments;
    BOOL ext_json; /* true if accepting JSON superset */
} JSParseState;

typedef struct JSOpCode {
#ifdef DUMP_BYTECODE
    const char *name;
#endif
    uint8_t size; /* in bytes */
    /* the opcodes remove n_pop items from the top of the stack, then
       pushes n_push items */
    uint8_t n_pop;
    uint8_t n_push;
    uint8_t fmt;
} JSOpCode;

extern const JSOpCode opcode_info[OP_COUNT + (OP_TEMP_END - OP_TEMP_START)];

#if SHORT_OPCODES
/* After the final compilation pass, short opcodes are used. Their
   opcodes overlap with the temporary opcodes which cannot appear in
   the final bytecode. Their description is after the temporary
   opcodes in opcode_info[]. */
#define short_opcode_info(op)           \
    opcode_info[(op) >= OP_TEMP_START ? \
                (op) + (OP_TEMP_END - OP_TEMP_START) : (op)]
#else
#define short_opcode_info(op) opcode_info[op]
#endif

void free_token(JSParseState *s, JSToken *token);

int __attribute__((format(printf, 2, 3))) js_parse_error(JSParseState *s, const char *fmt, ...);

__exception int js_parse_string(JSParseState *s, int sep,
                                BOOL do_throw, const uint8_t *p,
                                JSToken *token, const uint8_t **pp);

__exception int ident_realloc(JSContext *ctx, char **pbuf, size_t *psize,
                              char *static_buf);

int simple_next_token(const uint8_t **pp, BOOL no_line_terminator);

int push_scope(JSParseState *s);

JSModuleDef *js_new_module_def(JSContext *ctx, JSAtom name);

void js_free_module_def(JSContext *ctx, JSModuleDef *m);

JSExportEntry *add_export_entry2(JSContext *ctx,
                                 JSParseState *s, JSModuleDef *m,
                                 JSAtom local_name, JSAtom export_name,
                                 JSExportTypeEnum export_type);

static inline
JSExportEntry *add_export_entry(JSParseState *s, JSModuleDef *m,
                                JSAtom local_name, JSAtom export_name,
                                JSExportTypeEnum export_type)
{
    return add_export_entry2(s->ctx, s, m, local_name, export_name,
                             export_type);
}

typedef enum JSResolveResultEnum {
    JS_RESOLVE_RES_EXCEPTION = -1, /* memory alloc error */
    JS_RESOLVE_RES_FOUND = 0,
    JS_RESOLVE_RES_NOT_FOUND,
    JS_RESOLVE_RES_CIRCULAR,
    JS_RESOLVE_RES_AMBIGUOUS,
} JSResolveResultEnum;

JSResolveResultEnum js_resolve_export(JSContext *ctx,
                                      JSModuleDef **pmodule,
                                      JSExportEntry **pme,
                                      JSModuleDef *m,
                                      JSAtom export_name);

void js_resolve_export_throw_error(JSContext *ctx,
                                   JSResolveResultEnum res,
                                   JSModuleDef *m, JSAtom export_name);

JSValue js_get_module_ns(JSContext *ctx, JSModuleDef *m);

int js_resolve_module(JSContext *ctx, JSModuleDef *m);

int js_create_module_function(JSContext *ctx, JSModuleDef *m);

int js_link_module(JSContext *ctx, JSModuleDef *m);

JSValue js_import_meta(JSContext *ctx);

JSValue js_dynamic_import(JSContext *ctx, JSValueConst specifier);

JSValue js_evaluate_module(JSContext *ctx, JSModuleDef *m);

JSFunctionDef *js_new_function_def(JSContext *ctx,
                                   JSFunctionDef *parent,
                                   BOOL is_eval,
                                   BOOL is_func_expr,
                                   const char *filename, int line_num);

void free_bytecode_atoms(JSRuntime *rt,
                         const uint8_t *bc_buf, int bc_len,
                         BOOL use_short_opcodes);

void js_free_function_def(JSContext *ctx, JSFunctionDef *fd);

/* XXX: should handle the argument scope generically */
static inline
BOOL is_var_in_arg_scope(const JSVarDef *vd)
{
    return (vd->var_name == JS_ATOM_home_object ||
            vd->var_name == JS_ATOM_this_active_func ||
            vd->var_name == JS_ATOM_new_target ||
            vd->var_name == JS_ATOM_this ||
            vd->var_name == JS_ATOM__arg_var_ ||
            vd->var_kind == JS_VAR_FUNCTION_NAME);
}

JSValue js_create_function(JSContext *ctx, JSFunctionDef *fd);

__exception int js_parse_program(JSParseState *s);

void js_parse_init(JSContext *ctx, JSParseState *s,
                   const char *input, size_t input_len,
                   const char *filename);

static inline int check_function(JSContext *ctx, JSValueConst obj)
{
    if (likely(JS_IsFunction(ctx, obj)))
        return 0;
    JS_ThrowTypeError(ctx, "not a function");
    return -1;
}

static inline int check_exception_free(JSContext *ctx, JSValue obj)
{
    JS_FreeValue(ctx, obj);
    return JS_IsException(obj);
}

void JS_NewGlobalCConstructor2(JSContext *ctx,
                               JSValue func_obj,
                               const char *name,
                               JSValueConst proto);

JSValueConst JS_NewGlobalCConstructor(JSContext *ctx, const char *name,
                                      JSCFunction *func, int length,
                                      JSValueConst proto);

JSValueConst JS_NewGlobalCConstructorOnly(JSContext *ctx, const char *name,
                                          JSCFunction *func, int length,
                                          JSValueConst proto);

JSValue js_global_isNaN(JSContext *ctx, JSValueConst this_val,
                        int argc, JSValueConst *argv);

JSValue js_global_isFinite(JSContext *ctx, JSValueConst this_val,
                           int argc, JSValueConst *argv);

/* Object class */

int js_obj_to_desc(JSContext *ctx, JSPropertyDescriptor *d,
                   JSValueConst desc);
JSValue js_object_getPrototypeOf(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int magic);
JSValue js_object_defineProperty(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int magic);
JSValue js_object_getOwnPropertyDescriptor(JSContext *ctx, JSValueConst this_val,
                                           int argc, JSValueConst *argv, int magic);
JSValue JS_GetOwnPropertyNames2(JSContext *ctx, JSValueConst obj1,
                                int flags, int kind);
JSValue js_object_keys(JSContext *ctx, JSValueConst this_val,
                       int argc, JSValueConst *argv, int kind);
JSValue js_object_isExtensible(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv, int reflect);
JSValue js_object_preventExtensions(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv, int reflect);
JSValue js_object_toString(JSContext *ctx, JSValueConst this_val,
                           int argc, JSValueConst *argv);
JSValue js_object_seal(JSContext *ctx, JSValueConst this_val,
                       int argc, JSValueConst *argv, int freeze_flag);

static inline JSValue js_object_is(JSContext *ctx, JSValueConst this_val,
                            int argc, JSValueConst *argv)
{
    return JS_NewBool(ctx, js_same_value(ctx, argv[0], argv[1]));
}

JSValue JS_SpeciesConstructor(JSContext *ctx, JSValueConst obj,
                              JSValueConst defaultConstructor);

/* Function class */

JSValue js_function_constructor(JSContext *ctx, JSValueConst new_target,
                                int argc, JSValueConst *argv, int magic);
JSValue js_function_proto(JSContext *ctx, JSValueConst this_val,
                          int argc, JSValueConst *argv);
JSValue js_function_proto_caller(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv);

static inline __exception
int js_get_length32(JSContext *ctx, uint32_t *pres, JSValueConst obj)
{
    JSValue len_val;
    len_val = JS_GetProperty(ctx, obj, JS_ATOM_length);
    if (JS_IsException(len_val)) {
        *pres = 0;
        return -1;
    }
    return JS_ToUint32Free(ctx, pres, len_val);
}

static inline __exception
int js_get_length64(JSContext *ctx, int64_t *pres, JSValueConst obj)
{
    JSValue len_val;
    len_val = JS_GetProperty(ctx, obj, JS_ATOM_length);
    if (JS_IsException(len_val)) {
        *pres = 0;
        return -1;
    }
    return JS_ToLengthFree(ctx, pres, len_val);
}

JSValue js_error_constructor(JSContext *ctx, JSValueConst new_target,
                             int argc, JSValueConst *argv, int magic);

/* AggregateError */

JSValue js_aggregate_error_constructor(JSContext *ctx,
                                       JSValueConst errors);

/* Array */

static inline JSValue js_get_this(JSContext *ctx, JSValueConst this_val)
{
    return JS_DupValue(ctx, this_val);
}


#define special_every    0
#define special_some     1
#define special_forEach  2
#define special_map      3
#define special_filter   4
#define special_TA       8

JSValue js_typed_array___speciesCreate(JSContext *ctx,
                                       JSValueConst this_val,
                                       int argc, JSValueConst *argv);

JSValue js_array_every(JSContext *ctx, JSValueConst this_val,
                       int argc, JSValueConst *argv, int special);

#define special_reduce       0
#define special_reduceRight  1

JSValue js_array_reduce(JSContext *ctx, JSValueConst this_val,
                        int argc, JSValueConst *argv, int special);

JSValue js_array_includes(JSContext *ctx, JSValueConst this_val,
                          int argc, JSValueConst *argv);

JSValue js_array_pop(JSContext *ctx, JSValueConst this_val,
                     int argc, JSValueConst *argv, int shift);

JSValue js_array_push(JSContext *ctx, JSValueConst this_val,
                      int argc, JSValueConst *argv, int unshift);

typedef struct JSArrayIteratorData {
    JSValue obj;
    JSIteratorKindEnum kind;
    uint32_t idx;
} JSArrayIteratorData;

JSValue js_create_array(JSContext *ctx, int len, JSValueConst *tab);

JSValue js_array_iterator_next(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv,
                               BOOL *pdone, int magic);

JSValue js_iterator_proto_iterator(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv);

int js_get_radix(JSContext *ctx, JSValueConst val);

JSValue js_parseInt(JSContext *ctx, JSValueConst this_val,
                    int argc, JSValueConst *argv);

JSValue js_parseFloat(JSContext *ctx, JSValueConst this_val,
                      int argc, JSValueConst *argv);

/* String */

extern const JSClassExoticMethods js_string_exotic_methods;

JSValue js_string_constructor(JSContext *ctx, JSValueConst new_target,
                              int argc, JSValueConst *argv);

int string_indexof_char(JSString *p, int c, int from);

int64_t string_advance_index(JSString *p, int64_t index, BOOL unicode);

int js_is_regexp(JSContext *ctx, JSValueConst obj);

JSValue js_string___GetSubstitution(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv);

/* Proxy */

void js_proxy_finalizer(JSRuntime *rt, JSValue val);
void js_proxy_mark(JSRuntime *rt, JSValueConst val,
                   JS_MarkFunc *mark_func);

static inline JSValue JS_ThrowTypeErrorRevokedProxy(JSContext *ctx)
{
    return JS_ThrowTypeError(ctx, "revoked proxy");
}

JSValue js_proxy_getPrototypeOf(JSContext *ctx, JSValueConst obj);

int js_proxy_setPrototypeOf(JSContext *ctx, JSValueConst obj,
                            JSValueConst proto_val, BOOL throw_flag);

int js_proxy_isExtensible(JSContext *ctx, JSValueConst obj);

int js_proxy_preventExtensions(JSContext *ctx, JSValueConst obj);

int js_proxy_isArray(JSContext *ctx, JSValueConst obj);


void js_map_finalizer(JSRuntime *rt, JSValue val);
void js_map_mark(JSRuntime *rt, JSValueConst val,
                         JS_MarkFunc *mark_func);
void js_map_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_map_iterator_mark(JSRuntime *rt, JSValueConst val,
                         JS_MarkFunc *mark_func);
void reset_weak_ref(JSRuntime *rt, JSObject *p);

JSValue JS_CreateAsyncFromSyncIterator(JSContext *ctx,
                                       JSValueConst sync_iter);

/* global object */
JSValue js___date_clock(JSContext *ctx, JSValueConst this_val,
                        int argc, JSValueConst *argv);

enum {
    MATH_OP_ABS,
    MATH_OP_FLOOR,
    MATH_OP_CEIL,
    MATH_OP_ROUND,
    MATH_OP_TRUNC,
    MATH_OP_SQRT,
    MATH_OP_FPROUND,
    MATH_OP_ACOS,
    MATH_OP_ASIN,
    MATH_OP_ATAN,
    MATH_OP_ATAN2,
    MATH_OP_COS,
    MATH_OP_EXP,
    MATH_OP_LOG,
    MATH_OP_POW,
    MATH_OP_SIN,
    MATH_OP_TAN,
    MATH_OP_FMOD,
    MATH_OP_REM,
    MATH_OP_SIGN,

    MATH_OP_ADD,
    MATH_OP_SUB,
    MATH_OP_MUL,
    MATH_OP_DIV,
};

void js_float_env_finalizer(JSRuntime *rt, JSValue val);

/* Typed Arrays */

JSValue js_array_buffer_constructor3_shared(JSContext *ctx,
                                            JSValueConst new_target,
                                            uint64_t len,
                                            uint8_t *buf,
                                            JSFreeArrayBufferDataFunc *free_func,
                                            void *opaque, BOOL alloc_flag);
JSArrayBuffer *js_get_array_buffer(JSContext *ctx, JSValueConst obj);
JSValue js_typed_array_constructor(JSContext *ctx,
                                   JSValueConst this_val,
                                   int argc, JSValueConst *argv,
                                   int classid);

static JSValue JS_ThrowTypeErrorDetachedArrayBuffer(JSContext *ctx)
{
    return JS_ThrowTypeError(ctx, "ArrayBuffer is detached");
}

static inline
JSObject *get_typed_array(JSContext *ctx,
                          JSValueConst this_val,
                          int is_dataview)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(this_val) != JS_TAG_OBJECT)
        goto fail;
    p = JS_VALUE_GET_OBJ(this_val);
    if (is_dataview) {
        if (p->class_id != JS_CLASS_DATAVIEW)
            goto fail;
    } else {
        if (!(p->class_id >= JS_CLASS_UINT8C_ARRAY &&
              p->class_id <= JS_CLASS_FLOAT64_ARRAY)) {
        fail:
            JS_ThrowTypeError(ctx, "not a %s", is_dataview ? "DataView" : "TypedArray");
            return NULL;
        }
    }
    return p;
}

/* WARNING: 'p' must be a typed array */
static inline
BOOL typed_array_is_detached(JSContext *ctx, JSObject *p)
{
    JSTypedArray *ta = p->u.typed_array;
    JSArrayBuffer *abuf = ta->buffer->u.array_buffer;
    /* XXX: could simplify test by ensuring that
       p->u.array.u.ptr is NULL iff it is detached */
    return abuf->detached;
}

/* WARNING: 'p' must be a typed array. Works even if the array buffer
   is detached */
static inline
uint32_t typed_array_get_length(JSContext *ctx, JSObject *p)
{
    JSTypedArray *ta = p->u.typed_array;
    int size_log2 = typed_array_size_log2(p->class_id);
    return ta->length >> size_log2;
}

/* return < 0 if exception */
static inline
int js_typed_array_get_length_internal(JSContext *ctx, JSValueConst obj)
{
    JSObject *p;
    p = get_typed_array(ctx, obj, 0);
    if (!p)
        return -1;
    if (typed_array_is_detached(ctx, p)) {
        JS_ThrowTypeErrorDetachedArrayBuffer(ctx);
        return -1;
    }
    return p->u.array.count;
}
