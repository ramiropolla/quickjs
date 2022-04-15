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

#include "quickjs_internal.h"

/* return JS_ATOM_NULL in case of exception */
JSAtom JS_ValueToAtom(JSContext *ctx, JSValueConst val)
{
    JSAtom atom;
    uint32_t tag;
    tag = JS_VALUE_GET_TAG(val);
    if (tag == JS_TAG_INT &&
        (uint32_t)JS_VALUE_GET_INT(val) <= JS_ATOM_MAX_INT) {
        /* fast path for integer values */
        atom = __JS_AtomFromUInt32(JS_VALUE_GET_INT(val));
    } else if (tag == JS_TAG_SYMBOL) {
        JSAtomStruct *p = JS_VALUE_GET_PTR(val);
        atom = JS_DupAtom(ctx, js_get_atom_index(ctx->rt, p));
    } else {
        JSValue str;
        str = JS_ToPropertyKey(ctx, val);
        if (JS_IsException(str))
            return JS_ATOM_NULL;
        if (JS_VALUE_GET_TAG(str) == JS_TAG_SYMBOL) {
            atom = js_symbol_to_atom(ctx, str);
        } else {
            atom = JS_NewAtomStr(ctx, JS_VALUE_GET_STRING(str));
        }
    }
    return atom;
}

void JS_DumpAtoms(JSRuntime *rt)
{
    JSAtomStruct *p;
    int h, i;
    /* This only dumps hashed atoms, not JS_ATOM_TYPE_SYMBOL atoms */
    printf("JSAtom count=%d size=%d hash_size=%d:\n",
           rt->atom_count, rt->atom_size, rt->atom_hash_size);
    printf("JSAtom hash table: {\n");
    for(i = 0; i < rt->atom_hash_size; i++) {
        h = rt->atom_hash[i];
        if (h) {
            printf("  %d:", i);
            while (h) {
                p = rt->atom_array[h];
                printf(" ");
                JS_DumpString(rt, p);
                h = p->hash_next;
            }
            printf("\n");
        }
    }
    printf("}\n");
    printf("JSAtom table: {\n");
    for(i = 0; i < rt->atom_size; i++) {
        p = rt->atom_array[i];
        if (!atom_is_free(p)) {
            printf("  %d: { %d %08x ", i, p->atom_type, p->hash);
            if (!(p->len == 0 && p->is_wide_char != 0))
                JS_DumpString(rt, p);
            printf(" %d }\n", p->hash_next);
        }
    }
    printf("}\n");
}

static int JS_ResizeAtomHash(JSRuntime *rt, int new_hash_size)
{
    JSAtomStruct *p;
    uint32_t new_hash_mask, h, i, hash_next1, j, *new_hash;

    assert((new_hash_size & (new_hash_size - 1)) == 0); /* power of two */
    new_hash_mask = new_hash_size - 1;
    new_hash = js_mallocz_rt(rt, sizeof(rt->atom_hash[0]) * new_hash_size);
    if (!new_hash)
        return -1;
    for(i = 0; i < rt->atom_hash_size; i++) {
        h = rt->atom_hash[i];
        while (h != 0) {
            p = rt->atom_array[h];
            hash_next1 = p->hash_next;
            /* add in new hash table */
            j = p->hash & new_hash_mask;
            p->hash_next = new_hash[j];
            new_hash[j] = h;
            h = hash_next1;
        }
    }
    js_free_rt(rt, rt->atom_hash);
    rt->atom_hash = new_hash;
    rt->atom_hash_size = new_hash_size;
    rt->atom_count_resize = JS_ATOM_COUNT_RESIZE(new_hash_size);
    //    JS_DumpAtoms(rt);
    return 0;
}

static const char js_atom_init[] =
#define DEF(name, str) str "\0"
#include "quickjs-atom.h"
#undef DEF
;

int JS_InitAtoms(JSRuntime *rt)
{
    int i, len, atom_type;
    const char *p;

    rt->atom_hash_size = 0;
    rt->atom_hash = NULL;
    rt->atom_count = 0;
    rt->atom_size = 0;
    rt->atom_free_index = 0;
    if (JS_ResizeAtomHash(rt, 256))     /* there are at least 195 predefined atoms */
        return -1;

    p = js_atom_init;
    for(i = 1; i < JS_ATOM_END; i++) {
        if (i == JS_ATOM_Private_brand)
            atom_type = JS_ATOM_TYPE_PRIVATE;
        else if (i >= JS_ATOM_Symbol_toPrimitive)
            atom_type = JS_ATOM_TYPE_SYMBOL;
        else
            atom_type = JS_ATOM_TYPE_STRING;
        len = strlen(p);
        if (__JS_NewAtomInit(rt, p, len, atom_type) == JS_ATOM_NULL)
            return -1;
        p = p + len + 1;
    }
    return 0;
}

JSAtom JS_DupAtom(JSContext *ctx, JSAtom v)
{
    JSRuntime *rt;
    JSAtomStruct *p;

    if (!__JS_AtomIsConst(v)) {
        rt = ctx->rt;
        p = rt->atom_array[v];
        p->header.ref_count++;
    }
    return v;
}

JSAtomKindEnum JS_AtomGetKind(JSContext *ctx, JSAtom v)
{
    JSRuntime *rt;
    JSAtomStruct *p;

    rt = ctx->rt;
    if (__JS_AtomIsTaggedInt(v))
        return JS_ATOM_KIND_STRING;
    p = rt->atom_array[v];
    switch(p->atom_type) {
    case JS_ATOM_TYPE_STRING:
        return JS_ATOM_KIND_STRING;
    case JS_ATOM_TYPE_GLOBAL_SYMBOL:
        return JS_ATOM_KIND_SYMBOL;
    case JS_ATOM_TYPE_SYMBOL:
        switch(p->hash) {
        case JS_ATOM_HASH_SYMBOL:
            return JS_ATOM_KIND_SYMBOL;
        case JS_ATOM_HASH_PRIVATE:
            return JS_ATOM_KIND_PRIVATE;
        default:
            abort();
        }
    default:
        abort();
    }
}

JSAtom js_get_atom_index(JSRuntime *rt, JSAtomStruct *p)
{
    uint32_t i = p->hash_next;  /* atom_index */
    if (p->atom_type != JS_ATOM_TYPE_SYMBOL) {
        JSAtomStruct *p1;

        i = rt->atom_hash[p->hash & (rt->atom_hash_size - 1)];
        p1 = rt->atom_array[i];
        while (p1 != p) {
            assert(i != 0);
            i = p1->hash_next;
            p1 = rt->atom_array[i];
        }
    }
    return i;
}

/* string case (internal). Return JS_ATOM_NULL if error. 'str' is
   freed. */
JSAtom __JS_NewAtom(JSRuntime *rt, JSString *str, int atom_type)
{
    uint32_t h, h1, i;
    JSAtomStruct *p;
    int len;

#if 0
    printf("__JS_NewAtom: ");  JS_DumpString(rt, str); printf("\n");
#endif
    if (atom_type < JS_ATOM_TYPE_SYMBOL) {
        /* str is not NULL */
        if (str->atom_type == atom_type) {
            /* str is the atom, return its index */
            i = js_get_atom_index(rt, str);
            /* reduce string refcount and increase atom's unless constant */
            if (__JS_AtomIsConst(i))
                str->header.ref_count--;
            return i;
        }
        /* try and locate an already registered atom */
        len = str->len;
        h = hash_string(str, atom_type);
        h &= JS_ATOM_HASH_MASK;
        h1 = h & (rt->atom_hash_size - 1);
        i = rt->atom_hash[h1];
        while (i != 0) {
            p = rt->atom_array[i];
            if (p->hash == h &&
                p->atom_type == atom_type &&
                p->len == len &&
                js_string_memcmp(p, str, len) == 0) {
                if (!__JS_AtomIsConst(i))
                    p->header.ref_count++;
                goto done;
            }
            i = p->hash_next;
        }
    } else {
        h1 = 0; /* avoid warning */
        if (atom_type == JS_ATOM_TYPE_SYMBOL) {
            h = JS_ATOM_HASH_SYMBOL;
        } else {
            h = JS_ATOM_HASH_PRIVATE;
            atom_type = JS_ATOM_TYPE_SYMBOL;
        }
    }

    if (rt->atom_free_index == 0) {
        /* allow new atom entries */
        uint32_t new_size, start;
        JSAtomStruct **new_array;

        /* alloc new with size progression 3/2:
           4 6 9 13 19 28 42 63 94 141 211 316 474 711 1066 1599 2398 3597 5395 8092
           preallocating space for predefined atoms (at least 195).
         */
        new_size = max_int(211, rt->atom_size * 3 / 2);
        if (new_size > JS_ATOM_MAX)
            goto fail;
        /* XXX: should use realloc2 to use slack space */
        new_array = js_realloc_rt(rt, rt->atom_array, sizeof(*new_array) * new_size);
        if (!new_array)
            goto fail;
        /* Note: the atom 0 is not used */
        start = rt->atom_size;
        if (start == 0) {
            /* JS_ATOM_NULL entry */
            p = js_mallocz_rt(rt, sizeof(JSAtomStruct));
            if (!p) {
                js_free_rt(rt, new_array);
                goto fail;
            }
            p->header.ref_count = 1;  /* not refcounted */
            p->atom_type = JS_ATOM_TYPE_SYMBOL;
#ifdef DUMP_LEAKS
            list_add_tail(&p->link, &rt->string_list);
#endif
            new_array[0] = p;
            rt->atom_count++;
            start = 1;
        }
        rt->atom_size = new_size;
        rt->atom_array = new_array;
        rt->atom_free_index = start;
        for(i = start; i < new_size; i++) {
            uint32_t next;
            if (i == (new_size - 1))
                next = 0;
            else
                next = i + 1;
            rt->atom_array[i] = atom_set_free(next);
        }
    }

    if (str) {
        if (str->atom_type == 0) {
            p = str;
            p->atom_type = atom_type;
        } else {
            p = js_malloc_rt(rt, sizeof(JSString) +
                             (str->len << str->is_wide_char) +
                             1 - str->is_wide_char);
            if (unlikely(!p))
                goto fail;
            p->header.ref_count = 1;
            p->is_wide_char = str->is_wide_char;
            p->len = str->len;
#ifdef DUMP_LEAKS
            list_add_tail(&p->link, &rt->string_list);
#endif
            memcpy(p->u.str8, str->u.str8, (str->len << str->is_wide_char) +
                   1 - str->is_wide_char);
            js_free_string(rt, str);
        }
    } else {
        p = js_malloc_rt(rt, sizeof(JSAtomStruct)); /* empty wide string */
        if (!p)
            return JS_ATOM_NULL;
        p->header.ref_count = 1;
        p->is_wide_char = 1;    /* Hack to represent NULL as a JSString */
        p->len = 0;
#ifdef DUMP_LEAKS
        list_add_tail(&p->link, &rt->string_list);
#endif
    }

    /* use an already free entry */
    i = rt->atom_free_index;
    rt->atom_free_index = atom_get_free(rt->atom_array[i]);
    rt->atom_array[i] = p;

    p->hash = h;
    p->hash_next = i;   /* atom_index */
    p->atom_type = atom_type;

    rt->atom_count++;

    if (atom_type != JS_ATOM_TYPE_SYMBOL) {
        p->hash_next = rt->atom_hash[h1];
        rt->atom_hash[h1] = i;
        if (unlikely(rt->atom_count >= rt->atom_count_resize))
            JS_ResizeAtomHash(rt, rt->atom_hash_size * 2);
    }

    //    JS_DumpAtoms(rt);
    return i;

 fail:
    i = JS_ATOM_NULL;
 done:
    if (str)
        js_free_string(rt, str);
    return i;
}

/* only works with zero terminated 8 bit strings */
JSAtom __JS_NewAtomInit(JSRuntime *rt, const char *str, int len, int atom_type)
{
    JSString *p;
    p = js_alloc_string_rt(rt, len, 0);
    if (!p)
        return JS_ATOM_NULL;
    memcpy(p->u.str8, str, len);
    p->u.str8[len] = '\0';
    return __JS_NewAtom(rt, p, atom_type);
}

JSAtom __JS_FindAtom(JSRuntime *rt, const char *str, size_t len, int atom_type)
{
    uint32_t h, h1, i;
    JSAtomStruct *p;

    h = hash_string8((const uint8_t *)str, len, JS_ATOM_TYPE_STRING);
    h &= JS_ATOM_HASH_MASK;
    h1 = h & (rt->atom_hash_size - 1);
    i = rt->atom_hash[h1];
    while (i != 0) {
        p = rt->atom_array[i];
        if (p->hash == h &&
            p->atom_type == JS_ATOM_TYPE_STRING &&
            p->len == len &&
            p->is_wide_char == 0 &&
            memcmp(p->u.str8, str, len) == 0) {
            if (!__JS_AtomIsConst(i))
                p->header.ref_count++;
            return i;
        }
        i = p->hash_next;
    }
    return JS_ATOM_NULL;
}

void JS_FreeAtomStruct(JSRuntime *rt, JSAtomStruct *p)
{
#if 0   /* JS_ATOM_NULL is not refcounted: __JS_AtomIsConst() includes 0 */
    if (unlikely(i == JS_ATOM_NULL)) {
        p->header.ref_count = INT32_MAX / 2;
        return;
    }
#endif
    uint32_t i = p->hash_next;  /* atom_index */
    if (p->atom_type != JS_ATOM_TYPE_SYMBOL) {
        JSAtomStruct *p0, *p1;
        uint32_t h0;

        h0 = p->hash & (rt->atom_hash_size - 1);
        i = rt->atom_hash[h0];
        p1 = rt->atom_array[i];
        if (p1 == p) {
            rt->atom_hash[h0] = p1->hash_next;
        } else {
            for(;;) {
                assert(i != 0);
                p0 = p1;
                i = p1->hash_next;
                p1 = rt->atom_array[i];
                if (p1 == p) {
                    p0->hash_next = p1->hash_next;
                    break;
                }
            }
        }
    }
    /* insert in free atom list */
    rt->atom_array[i] = atom_set_free(rt->atom_free_index);
    rt->atom_free_index = i;
    /* free the string structure */
#ifdef DUMP_LEAKS
    list_del(&p->link);
#endif
    js_free_rt(rt, p);
    rt->atom_count--;
    assert(rt->atom_count >= 0);
}

static void __JS_FreeAtom(JSRuntime *rt, uint32_t i)
{
    JSAtomStruct *p;

    p = rt->atom_array[i];
    if (--p->header.ref_count > 0)
        return;
    JS_FreeAtomStruct(rt, p);
}

/* return TRUE if the string is a number n with 0 <= n <= 2^32-1 */
static inline BOOL is_num_string(uint32_t *pval, const JSString *p)
{
    uint32_t n;
    uint64_t n64;
    int c, i, len;

    len = p->len;
    if (len == 0 || len > 10)
        return FALSE;
    if (p->is_wide_char)
        c = p->u.str16[0];
    else
        c = p->u.str8[0];
    if (is_num(c)) {
        if (c == '0') {
            if (len != 1)
                return FALSE;
            n = 0;
        } else {
            n = c - '0';
            for(i = 1; i < len; i++) {
                if (p->is_wide_char)
                    c = p->u.str16[i];
                else
                    c = p->u.str8[i];
                if (!is_num(c))
                    return FALSE;
                n64 = (uint64_t)n * 10 + (c - '0');
                if ((n64 >> 32) != 0)
                    return FALSE;
                n = n64;
            }
        }
        *pval = n;
        return TRUE;
    } else {
        return FALSE;
    }
}

/* Warning: 'p' is freed */
JSAtom JS_NewAtomStr(JSContext *ctx, JSString *p)
{
    JSRuntime *rt = ctx->rt;
    uint32_t n;
    if (is_num_string(&n, p)) {
        if (n <= JS_ATOM_MAX_INT) {
            js_free_string(rt, p);
            return __JS_AtomFromUInt32(n);
        }
    }
    /* XXX: should generate an exception */
    return __JS_NewAtom(rt, p, JS_ATOM_TYPE_STRING);
}

JSAtom JS_NewAtomLen(JSContext *ctx, const char *str, size_t len)
{
    JSValue val;

    if (len == 0 || !is_digit(*str)) {
        JSAtom atom = __JS_FindAtom(ctx->rt, str, len, JS_ATOM_TYPE_STRING);
        if (atom)
            return atom;
    }
    val = JS_NewStringLen(ctx, str, len);
    if (JS_IsException(val))
        return JS_ATOM_NULL;
    return JS_NewAtomStr(ctx, JS_VALUE_GET_STRING(val));
}

JSAtom JS_NewAtom(JSContext *ctx, const char *str)
{
    return JS_NewAtomLen(ctx, str, strlen(str));
}

JSAtom JS_NewAtomUInt32(JSContext *ctx, uint32_t n)
{
    if (n <= JS_ATOM_MAX_INT) {
        return __JS_AtomFromUInt32(n);
    } else {
        char buf[11];
        JSValue val;
        snprintf(buf, sizeof(buf), "%u", n);
        val = JS_NewString(ctx, buf);
        if (JS_IsException(val))
            return JS_ATOM_NULL;
        return __JS_NewAtom(ctx->rt, JS_VALUE_GET_STRING(val),
                            JS_ATOM_TYPE_STRING);
    }
}

JSAtom JS_NewAtomInt64(JSContext *ctx, int64_t n)
{
    if ((uint64_t)n <= JS_ATOM_MAX_INT) {
        return __JS_AtomFromUInt32((uint32_t)n);
    } else {
        char buf[24];
        JSValue val;
        snprintf(buf, sizeof(buf), "%" PRId64 , n);
        val = JS_NewString(ctx, buf);
        if (JS_IsException(val))
            return JS_ATOM_NULL;
        return __JS_NewAtom(ctx->rt, JS_VALUE_GET_STRING(val),
                            JS_ATOM_TYPE_STRING);
    }
}

/* Should only be used for debug. */
const char *JS_AtomGetStrRT(JSRuntime *rt, char *buf, int buf_size,
                            JSAtom atom)
{
    if (__JS_AtomIsTaggedInt(atom)) {
        snprintf(buf, buf_size, "%u", __JS_AtomToUInt32(atom));
    } else {
        JSAtomStruct *p;
        assert(atom < rt->atom_size);
        if (atom == JS_ATOM_NULL) {
            snprintf(buf, buf_size, "<null>");
        } else {
            int i, c;
            char *q;
            JSString *str;

            q = buf;
            p = rt->atom_array[atom];
            assert(!atom_is_free(p));
            str = p;
            if (str) {
                if (!str->is_wide_char) {
                    /* special case ASCII strings */
                    c = 0;
                    for(i = 0; i < str->len; i++) {
                        c |= str->u.str8[i];
                    }
                    if (c < 0x80)
                        return (const char *)str->u.str8;
                }
                for(i = 0; i < str->len; i++) {
                    if (str->is_wide_char)
                        c = str->u.str16[i];
                    else
                        c = str->u.str8[i];
                    if ((q - buf) >= buf_size - UTF8_CHAR_LEN_MAX)
                        break;
                    if (c < 128) {
                        *q++ = c;
                    } else {
                        q += unicode_to_utf8((uint8_t *)q, c);
                    }
                }
            }
            *q = '\0';
        }
    }
    return buf;
}

static JSValue __JS_AtomToValue(JSContext *ctx, JSAtom atom, BOOL force_string)
{
    char buf[ATOM_GET_STR_BUF_SIZE];

    if (__JS_AtomIsTaggedInt(atom)) {
        snprintf(buf, sizeof(buf), "%u", __JS_AtomToUInt32(atom));
        return JS_NewString(ctx, buf);
    } else {
        JSRuntime *rt = ctx->rt;
        JSAtomStruct *p;
        assert(atom < rt->atom_size);
        p = rt->atom_array[atom];
        if (p->atom_type == JS_ATOM_TYPE_STRING) {
            goto ret_string;
        } else if (force_string) {
            if (p->len == 0 && p->is_wide_char != 0) {
                /* no description string */
                p = rt->atom_array[JS_ATOM_empty_string];
            }
        ret_string:
            return JS_DupValue(ctx, JS_MKPTR(JS_TAG_STRING, p));
        } else {
            return JS_DupValue(ctx, JS_MKPTR(JS_TAG_SYMBOL, p));
        }
    }
}

JSValue JS_AtomToValue(JSContext *ctx, JSAtom atom)
{
    return __JS_AtomToValue(ctx, atom, FALSE);
}

JSValue JS_AtomToString(JSContext *ctx, JSAtom atom)
{
    return __JS_AtomToValue(ctx, atom, TRUE);
}

/* return TRUE if the atom is an array index (i.e. 0 <= index <=
   2^32-2 and return its value */
BOOL JS_AtomIsArrayIndex(JSContext *ctx, uint32_t *pval, JSAtom atom)
{
    if (__JS_AtomIsTaggedInt(atom)) {
        *pval = __JS_AtomToUInt32(atom);
        return TRUE;
    } else {
        JSRuntime *rt = ctx->rt;
        JSAtomStruct *p;
        uint32_t val;

        assert(atom < rt->atom_size);
        p = rt->atom_array[atom];
        if (p->atom_type == JS_ATOM_TYPE_STRING &&
            is_num_string(&val, p) && val != -1) {
            *pval = val;
            return TRUE;
        } else {
            *pval = 0;
            return FALSE;
        }
    }
}

/* This test must be fast if atom is not a numeric index (e.g. a
   method name). Return JS_UNDEFINED if not a numeric
   index. JS_EXCEPTION can also be returned. */
JSValue JS_AtomIsNumericIndex1(JSContext *ctx, JSAtom atom)
{
    JSRuntime *rt = ctx->rt;
    JSAtomStruct *p1;
    JSString *p;
    int c, len, ret;
    JSValue num, str;

    if (__JS_AtomIsTaggedInt(atom))
        return JS_NewInt32(ctx, __JS_AtomToUInt32(atom));
    assert(atom < rt->atom_size);
    p1 = rt->atom_array[atom];
    if (p1->atom_type != JS_ATOM_TYPE_STRING)
        return JS_UNDEFINED;
    p = p1;
    len = p->len;
    if (p->is_wide_char) {
        const uint16_t *r = p->u.str16, *r_end = p->u.str16 + len;
        if (r >= r_end)
            return JS_UNDEFINED;
        c = *r;
        if (c == '-') {
            if (r >= r_end)
                return JS_UNDEFINED;
            r++;
            c = *r;
            /* -0 case is specific */
            if (c == '0' && len == 2)
                goto minus_zero;
        }
        /* XXX: should test NaN, but the tests do not check it */
        if (!is_num(c)) {
            /* XXX: String should be normalized, therefore 8-bit only */
            const uint16_t nfinity16[7] = { 'n', 'f', 'i', 'n', 'i', 't', 'y' };
            if (!(c =='I' && (r_end - r) == 8 &&
                  !memcmp(r + 1, nfinity16, sizeof(nfinity16))))
                return JS_UNDEFINED;
        }
    } else {
        const uint8_t *r = p->u.str8, *r_end = p->u.str8 + len;
        if (r >= r_end)
            return JS_UNDEFINED;
        c = *r;
        if (c == '-') {
            if (r >= r_end)
                return JS_UNDEFINED;
            r++;
            c = *r;
            /* -0 case is specific */
            if (c == '0' && len == 2) {
            minus_zero:
                return __JS_NewFloat64(ctx, -0.0);
            }
        }
        if (!is_num(c)) {
            if (!(c =='I' && (r_end - r) == 8 &&
                  !memcmp(r + 1, "nfinity", 7)))
                return JS_UNDEFINED;
        }
    }
    /* XXX: bignum: would be better to only accept integer to avoid
       relying on current floating point precision */
    /* this is ECMA CanonicalNumericIndexString primitive */
    num = JS_ToNumber(ctx, JS_MKPTR(JS_TAG_STRING, p));
    if (JS_IsException(num))
        return num;
    str = JS_ToString(ctx, num);
    if (JS_IsException(str)) {
        JS_FreeValue(ctx, num);
        return str;
    }
    ret = js_string_compare(ctx, p, JS_VALUE_GET_STRING(str));
    JS_FreeValue(ctx, str);
    if (ret == 0) {
        return num;
    } else {
        JS_FreeValue(ctx, num);
        return JS_UNDEFINED;
    }
}

/* return -1 if exception or TRUE/FALSE */
int JS_AtomIsNumericIndex(JSContext *ctx, JSAtom atom)
{
    JSValue num;
    num = JS_AtomIsNumericIndex1(ctx, atom);
    if (likely(JS_IsUndefined(num)))
        return FALSE;
    if (JS_IsException(num))
        return -1;
    JS_FreeValue(ctx, num);
    return TRUE;
}

void JS_FreeAtom(JSContext *ctx, JSAtom v)
{
    if (!__JS_AtomIsConst(v))
        __JS_FreeAtom(ctx->rt, v);
}

void JS_FreeAtomRT(JSRuntime *rt, JSAtom v)
{
    if (!__JS_AtomIsConst(v))
        __JS_FreeAtom(rt, v);
}

/* return TRUE if 'v' is a symbol with a string description */
BOOL JS_AtomSymbolHasDescription(JSContext *ctx, JSAtom v)
{
    JSRuntime *rt;
    JSAtomStruct *p;

    rt = ctx->rt;
    if (__JS_AtomIsTaggedInt(v))
        return FALSE;
    p = rt->atom_array[v];
    return (((p->atom_type == JS_ATOM_TYPE_SYMBOL &&
              p->hash == JS_ATOM_HASH_SYMBOL) ||
             p->atom_type == JS_ATOM_TYPE_GLOBAL_SYMBOL) &&
            !(p->len == 0 && p->is_wide_char != 0));
}

static __maybe_unused void print_atom(JSContext *ctx, JSAtom atom)
{
    char buf[ATOM_GET_STR_BUF_SIZE];
    const char *p;
    int i;

    /* XXX: should handle embedded null characters */
    /* XXX: should move encoding code to JS_AtomGetStr */
    p = JS_AtomGetStr(ctx, buf, sizeof(buf), atom);
    for (i = 0; p[i]; i++) {
        int c = (unsigned char)p[i];
        if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
              (c == '_' || c == '$') || (c >= '0' && c <= '9' && i > 0)))
            break;
    }
    if (i > 0 && p[i] == '\0') {
        printf("%s", p);
    } else {
        putchar('"');
        printf("%.*s", i, p);
        for (; p[i]; i++) {
            int c = (unsigned char)p[i];
            if (c == '\"' || c == '\\') {
                putchar('\\');
                putchar(c);
            } else if (c >= ' ' && c <= 126) {
                putchar(c);
            } else if (c == '\n') {
                putchar('\\');
                putchar('n');
            } else {
                printf("\\u%04x", c);
            }
        }
        putchar('\"');
    }
}

/* free with JS_FreeCString() */
const char *JS_AtomToCString(JSContext *ctx, JSAtom atom)
{
    JSValue str;
    const char *cstr;

    str = JS_AtomToString(ctx, atom);
    if (JS_IsException(str))
        return NULL;
    cstr = JS_ToCString(ctx, str);
    JS_FreeValue(ctx, str);
    return cstr;
}
