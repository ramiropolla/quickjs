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

/* Arguments */

static int js_arguments_define_own_property(JSContext *ctx,
                                            JSValueConst this_obj,
                                            JSAtom prop, JSValueConst val,
                                            JSValueConst getter, JSValueConst setter, int flags)
{
    JSObject *p;
    uint32_t idx;
    p = JS_VALUE_GET_OBJ(this_obj);
    /* convert to normal array when redefining an existing numeric field */
    if (p->fast_array && JS_AtomIsArrayIndex(ctx, &idx, prop) &&
        idx < p->u.array.count) {
        if (convert_fast_array_to_array(ctx, p))
            return -1;
    }
    /* run the default define own property */
    return JS_DefineProperty(ctx, this_obj, prop, val, getter, setter,
                             flags | JS_PROP_NO_EXOTIC);
}

const JSClassExoticMethods js_arguments_exotic_methods = {
    .define_own_property = js_arguments_define_own_property,
};

/* FastArray */

JSValue JS_NewFastArray(JSContext *ctx, JSValue **parray, uint32_t len)
{
    JSValue *new_array_prop;
    JSObject *p;
    size_t slack;

    /* create new array */
    JSValue val = JS_NewObjectFromShape(ctx, js_dup_shape(ctx->fast_array_shape),
                                        JS_CLASS_FAST_ARRAY);
    if ( JS_IsException(val) )
        return val;
    p = JS_VALUE_GET_OBJ(val);

    /* allocate array values */
    new_array_prop = js_realloc2(ctx, p->u.array.u.values, sizeof(JSValue) * len, &slack);
    if ( !new_array_prop )
    {
        JS_FreeValue(ctx, val);
        return JS_EXCEPTION;
    }
    for ( size_t i = 0; i < len; i++ )
        new_array_prop[i] = JS_UNDEFINED;

    p->prop[0].u.value = JS_NewInt32(ctx, len);
    p->u.array.u.values = new_array_prop;
    p->u.array.u1.size = len + (slack / sizeof(*new_array_prop));
    p->u.array.count = len;

    *parray = p->u.array.u.values;

    return val;
}

int JS_GetFastArray(JSValueConst val, JSValue **parray, uint32_t *plen)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    if ( p->class_id != JS_CLASS_FAST_ARRAY )
        return FALSE;
    *parray = p->u.array.u.values;
    *plen = p->u.array.count;
    return TRUE;
}

int JS_IsFastArray(JSContext *ctx, JSValueConst val)
{
    JSObject *p;
    if (JS_VALUE_GET_TAG(val) == JS_TAG_OBJECT) {
        p = JS_VALUE_GET_OBJ(val);
        if (unlikely(p->class_id == JS_CLASS_PROXY))
            return js_proxy_isFastArray(ctx, val);
        else
            return (p->class_id == JS_CLASS_FAST_ARRAY);
    } else {
        return FALSE;
    }
}

/* Array */

void js_array_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    int i;

    for(i = 0; i < p->u.array.count; i++) {
        JS_FreeValueRT(rt, p->u.array.u.values[i]);
    }
    js_free_rt(rt, p->u.array.u.values);
}

void js_array_mark(JSRuntime *rt, JSValueConst val,
                   JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    int i;

    for(i = 0; i < p->u.array.count; i++) {
        JS_MarkValue(rt, p->u.array.u.values[i], mark_func);
    }
}

/* set the array length and remove the array elements if necessary. */
int set_array_length(JSContext *ctx, JSObject *p, JSValue val, int flags)
{
    uint32_t len, idx, cur_len;
    int i, ret;

    /* Note: this call can reallocate the properties of 'p' */
    ret = JS_ToArrayLengthFree(ctx, &len, val, FALSE);
    if (ret)
        return -1;
    /* JS_ToArrayLengthFree() must be done before the read-only test */
    if (unlikely(!(p->shape->prop[0].flags & JS_PROP_WRITABLE)))
        return JS_ThrowTypeErrorReadOnly(ctx, flags, JS_ATOM_length);

    if (likely(p->fast_array)) {
        uint32_t old_len = p->u.array.count;
        if (len < old_len) {
            for(i = len; i < old_len; i++) {
                JS_FreeValue(ctx, p->u.array.u.values[i]);
            }
            p->u.array.count = len;
        }
        p->prop[0].u.value = JS_NewUint32(ctx, len);
    } else {
        /* Note: length is always a uint32 because the object is an
           array */
        JS_ToUint32Ptr(ctx, &cur_len, p->prop[0].u.value);
        if (len < cur_len) {
            uint32_t d;
            JSShape *sh;
            JSShapeProperty *pr;

            d = cur_len - len;
            sh = p->shape;
            if (d <= sh->prop_count) {
                JSAtom atom;

                /* faster to iterate */
                while (cur_len > len) {
                    atom = JS_NewAtomUInt32(ctx, cur_len - 1);
                    ret = delete_property(ctx, p, atom);
                    JS_FreeAtom(ctx, atom);
                    if (unlikely(!ret)) {
                        /* unlikely case: property is not
                           configurable */
                        break;
                    }
                    cur_len--;
                }
            } else {
                /* faster to iterate thru all the properties. Need two
                   passes in case one of the property is not
                   configurable */
                cur_len = len;
                for(i = 0, pr = get_shape_prop(sh); i < sh->prop_count;
                    i++, pr++) {
                    if (pr->atom != JS_ATOM_NULL &&
                        JS_AtomIsArrayIndex(ctx, &idx, pr->atom)) {
                        if (idx >= cur_len &&
                            !(pr->flags & JS_PROP_CONFIGURABLE)) {
                            cur_len = idx + 1;
                        }
                    }
                }

                for(i = 0, pr = get_shape_prop(sh); i < sh->prop_count;
                    i++, pr++) {
                    if (pr->atom != JS_ATOM_NULL &&
                        JS_AtomIsArrayIndex(ctx, &idx, pr->atom)) {
                        if (idx >= cur_len) {
                            /* remove the property */
                            delete_property(ctx, p, pr->atom);
                            /* WARNING: the shape may have been modified */
                            sh = p->shape;
                            pr = get_shape_prop(sh) + i;
                        }
                    }
                }
            }
        } else {
            cur_len = len;
        }
        set_value(ctx, &p->prop[0].u.value, JS_NewUint32(ctx, cur_len));
        if (unlikely(cur_len > len)) {
            return JS_ThrowTypeErrorOrFalse(ctx, flags, "not configurable");
        }
    }
    return TRUE;
}

/* return -1 if exception */
static int expand_fast_array(JSContext *ctx, JSObject *p, uint32_t new_len)
{
    uint32_t new_size;
    size_t slack;
    JSValue *new_array_prop;
    /* XXX: potential arithmetic overflow */
    new_size = max_int(new_len, p->u.array.u1.size * 3 / 2);
    new_array_prop = js_realloc2(ctx, p->u.array.u.values, sizeof(JSValue) * new_size, &slack);
    if (!new_array_prop)
        return -1;
    new_size += slack / sizeof(*new_array_prop);
    p->u.array.u.values = new_array_prop;
    p->u.array.u1.size = new_size;
    return 0;
}

/* Preconditions: 'p' must be of class JS_CLASS_ARRAY, p->fast_array =
   TRUE and p->extensible = TRUE */
int add_fast_array_element_internal(JSContext *ctx, JSObject *p, uint32_t new_len, JSValue val, int flags)
{
    uint32_t array_len;
    /* XXX: convert to slow array if new_len > 2^31-1 elements */
    /* update the length if necessary. We assume that if the length is
       not an integer, then if it >= 2^31.  */
    if (likely(JS_VALUE_GET_TAG(p->prop[0].u.value) == JS_TAG_INT)) {
        array_len = JS_VALUE_GET_INT(p->prop[0].u.value);
        if (new_len > array_len) {
            if (unlikely(!(get_shape_prop(p->shape)->flags & JS_PROP_WRITABLE))) {
                JS_FreeValue(ctx, val);
                return JS_ThrowTypeErrorReadOnly(ctx, flags, JS_ATOM_length);
            }
            p->prop[0].u.value = JS_NewInt32(ctx, new_len);
        }
    }
    if (unlikely(new_len > p->u.array.u1.size)) {
        if (expand_fast_array(ctx, p, new_len)) {
            JS_FreeValue(ctx, val);
            return -1;
        }
    }
    for ( uint32_t i = p->u.array.count; i + 1 < new_len; i++ )
        p->u.array.u.values[i] = JS_UNDEFINED;
    p->u.array.u.values[new_len - 1] = val;
    p->u.array.count = new_len;
    return TRUE;
}

int add_fast_array_element(JSContext *ctx, JSObject *p, JSValue val, int flags)
{
    uint32_t new_len;
    /* extend the array by one */
    /* XXX: convert to slow array if new_len > 2^31-1 elements */
    new_len = p->u.array.count + 1;
    return add_fast_array_element_internal(ctx, p, new_len, val, flags);
}

static int JS_CopySubArray(JSContext *ctx,
                           JSValueConst obj, int64_t to_pos,
                           int64_t from_pos, int64_t count, int dir)
{
    JSObject *p;
    int64_t i, from, to, len;
    JSValue val;
    int fromPresent;
    BOOL is_fast_array = FALSE;

    p = NULL;
    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        p = JS_VALUE_GET_OBJ(obj);
        is_fast_array = (p->class_id == JS_CLASS_FAST_ARRAY)
                     || ((p->class_id == JS_CLASS_ARRAY) && p->fast_array);
        if (!is_fast_array) {
            p = NULL;
        }
    }

    for (i = 0; i < count; ) {
        if (dir < 0) {
            from = from_pos + count - i - 1;
            to = to_pos + count - i - 1;
        } else {
            from = from_pos + i;
            to = to_pos + i;
        }
        if (is_fast_array &&
            from >= 0 && from < (len = p->u.array.count)  &&
            to >= 0 && to < len) {
            int64_t l, j;
            /* Fast path for fast arrays. Since we don't look at the
               prototype chain, we can optimize only the cases where
               all the elements are present in the array. */
            l = count - i;
            if (dir < 0) {
                l = min_int64(l, from + 1);
                l = min_int64(l, to + 1);
                for(j = 0; j < l; j++) {
                    set_value(ctx, &p->u.array.u.values[to - j],
                              JS_DupValue(ctx, p->u.array.u.values[from - j]));
                }
            } else {
                l = min_int64(l, len - from);
                l = min_int64(l, len - to);
                for(j = 0; j < l; j++) {
                    set_value(ctx, &p->u.array.u.values[to + j],
                              JS_DupValue(ctx, p->u.array.u.values[from + j]));
                }
            }
            i += l;
        } else {
            fromPresent = JS_TryGetPropertyInt64(ctx, obj, from, &val);
            if (fromPresent < 0)
                goto exception;

            if (fromPresent) {
                if (JS_SetPropertyInt64(ctx, obj, to, val) < 0)
                    goto exception;
            } else {
                if (JS_DeletePropertyInt64(ctx, obj, to, JS_PROP_THROW) < 0)
                    goto exception;
            }
            i++;
        }
    }
    return 0;

 exception:
    return -1;
}

static JSValue js_array_constructor_internal(JSContext *ctx, JSValueConst new_target,
                                             int argc, JSValueConst *argv, int class_id)
{
    JSValue obj;
    int i;

    obj = js_create_from_ctor(ctx, new_target, class_id);
    if (JS_IsException(obj))
        return obj;
    if (argc == 1 && JS_IsNumber(argv[0])) {
        uint32_t len;
        if (JS_ToArrayLengthFree(ctx, &len, JS_DupValue(ctx, argv[0]), TRUE))
            goto fail;
        if (JS_SetProperty(ctx, obj, JS_ATOM_length, JS_NewUint32(ctx, len)) < 0)
            goto fail;
    } else {
        if ( class_id == JS_CLASS_FAST_ARRAY )
        {
            if ( argc > 0 )
            {
                JSObject *p = JS_VALUE_GET_OBJ(obj);
                JSValue *arrp;
                if (expand_fast_array(ctx, p, argc))
                    goto fail;
                arrp = p->u.array.u.values;
                for ( i = 0; i < argc; i++ )
                    arrp[i] = JS_DupValue(ctx, argv[i]);
                p->u.array.count = argc;
                p->prop[0].u.value = JS_NewInt32(ctx, argc);
            }
        }
        else
        {
            for(i = 0; i < argc; i++) {
                if (JS_SetPropertyUint32(ctx, obj, i, JS_DupValue(ctx, argv[i])) < 0)
                    goto fail;
            }
        }
    }
    return obj;
fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_constructor(JSContext *ctx, JSValueConst new_target,
                                    int argc, JSValueConst *argv)
{
    return js_array_constructor_internal(ctx, new_target, argc, argv, JS_CLASS_ARRAY);
}

static JSValue js_fast_array_constructor(JSContext *ctx, JSValueConst new_target,
                                         int argc, JSValueConst *argv)
{
    return js_array_constructor_internal(ctx, new_target, argc, argv, JS_CLASS_FAST_ARRAY);
}

static JSValue js_array_from_internal(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv, int class_id)
{
    // from(items, mapfn = void 0, this_arg = void 0)
    JSValueConst items = argv[0], mapfn, this_arg;
    JSValueConst args[2];
    JSValue stack[2];
    JSValue iter, r, v, v2, arrayLike;
    int64_t k, len;
    int done, mapping;

    mapping = FALSE;
    mapfn = JS_UNDEFINED;
    this_arg = JS_UNDEFINED;
    r = JS_UNDEFINED;
    arrayLike = JS_UNDEFINED;
    stack[0] = JS_UNDEFINED;
    stack[1] = JS_UNDEFINED;

    if (argc > 1) {
        mapfn = argv[1];
        if (!JS_IsUndefined(mapfn)) {
            if (check_function(ctx, mapfn))
                goto exception;
            mapping = 1;
            if (argc > 2)
                this_arg = argv[2];
        }
    }
    iter = JS_GetProperty(ctx, items, JS_ATOM_Symbol_iterator);
    if (JS_IsException(iter))
        goto exception;
    if (!JS_IsUndefined(iter)) {
        JS_FreeValue(ctx, iter);
        if (JS_IsConstructor(ctx, this_val))
            r = JS_CallConstructor(ctx, this_val, 0, NULL);
        else if ( class_id == JS_CLASS_FAST_ARRAY )
            r = JS_NewFastArray(ctx, NULL, 0);
        else
            r = JS_NewArray(ctx);
        if (JS_IsException(r))
            goto exception;
        stack[0] = JS_DupValue(ctx, items);
        if (js_for_of_start(ctx, &stack[1], FALSE))
            goto exception;
        for (k = 0;; k++) {
            v = JS_IteratorNext(ctx, stack[0], stack[1], 0, NULL, &done);
            if (JS_IsException(v))
                goto exception_close;
            if (done)
                break;
            if (mapping) {
                args[0] = v;
                args[1] = JS_NewInt32(ctx, k);
                v2 = JS_Call(ctx, mapfn, this_arg, 2, args);
                JS_FreeValue(ctx, v);
                v = v2;
                if (JS_IsException(v))
                    goto exception_close;
            }
            if (JS_DefinePropertyValueInt64(ctx, r, k, v,
                                            JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                goto exception_close;
        }
    } else {
        arrayLike = JS_ToObject(ctx, items);
        if (JS_IsException(arrayLike))
            goto exception;
        if (js_get_length64(ctx, &len, arrayLike) < 0)
            goto exception;
        v = JS_NewInt64(ctx, len);
        args[0] = v;
        if (JS_IsConstructor(ctx, this_val)) {
            r = JS_CallConstructor(ctx, this_val, 1, args);
        } else {
            r = js_array_constructor(ctx, JS_UNDEFINED, 1, args);
        }
        JS_FreeValue(ctx, v);
        if (JS_IsException(r))
            goto exception;
        for(k = 0; k < len; k++) {
            v = JS_GetPropertyInt64(ctx, arrayLike, k);
            if (JS_IsException(v))
                goto exception;
            if (mapping) {
                args[0] = v;
                args[1] = JS_NewInt32(ctx, k);
                v2 = JS_Call(ctx, mapfn, this_arg, 2, args);
                JS_FreeValue(ctx, v);
                v = v2;
                if (JS_IsException(v))
                    goto exception;
            }
            if (JS_DefinePropertyValueInt64(ctx, r, k, v,
                                            JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                goto exception;
        }
    }
    if (JS_SetProperty(ctx, r, JS_ATOM_length, JS_NewUint32(ctx, k)) < 0)
        goto exception;
    goto done;

 exception_close:
    if (!JS_IsUndefined(stack[0]))
        JS_IteratorClose(ctx, stack[0], TRUE);
 exception:
    JS_FreeValue(ctx, r);
    r = JS_EXCEPTION;
 done:
    JS_FreeValue(ctx, arrayLike);
    JS_FreeValue(ctx, stack[0]);
    JS_FreeValue(ctx, stack[1]);
    return r;
}

static JSValue js_array_from(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    return js_array_from_internal(ctx, this_val, argc, argv, JS_CLASS_ARRAY);
}

static JSValue js_fast_array_from(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    return js_array_from_internal(ctx, this_val, argc, argv, JS_CLASS_FAST_ARRAY);
}

static JSValue js_array_of(JSContext *ctx, JSValueConst this_val,
                           int argc, JSValueConst *argv)
{
    JSValue obj, args[1];
    JSObject *p;
    int i;

    p = JS_VALUE_GET_OBJ(this_val);
    if (JS_IsConstructor(ctx, this_val)) {
        args[0] = JS_NewInt32(ctx, argc);
        obj = JS_CallConstructor(ctx, this_val, 1, (JSValueConst *)args);
    } else if ( p->class_id == JS_CLASS_FAST_ARRAY ) {
        obj = JS_NewFastArray(ctx, NULL, 0);
    } else {
        obj = JS_NewArray(ctx);
    }
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    for(i = 0; i < argc; i++) {
        // FIXME fast array optimize?
        if (JS_CreateDataPropertyUint32(ctx, obj, i, JS_DupValue(ctx, argv[i]),
                                        JS_PROP_THROW) < 0) {
            goto fail;
        }
    }
    if (JS_SetProperty(ctx, obj, JS_ATOM_length, JS_NewUint32(ctx, argc)) < 0) {
    fail:
        JS_FreeValue(ctx, obj);
        return JS_EXCEPTION;
    }
    return obj;
}

static JSValue js_array_isArray(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    int ret;
    ret = JS_IsArray(ctx, argv[0]);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_fast_array_isArray(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv)
{
    int ret;
    ret = JS_IsArray(ctx, argv[0]);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue JS_ArraySpeciesCreate(JSContext *ctx, JSValueConst obj,
                                    JSValueConst len_val)
{
    JSObject *p = JS_VALUE_GET_OBJ(obj);
    JSValue ctor, ret, species;
    int res;
    JSContext *realm;

    switch ( p->class_id )
    {
        case JS_CLASS_FAST_ARRAY: res = JS_IsFastArray(ctx, obj); break;
        default:                  res = JS_IsArray(ctx, obj);     break;
    }
    if (res < 0)
        return JS_EXCEPTION;
    if (!res)
    {
use_default_constructor:
        switch ( p->class_id )
        {
            case JS_CLASS_FAST_ARRAY: return js_fast_array_constructor(ctx, JS_UNDEFINED, 1, &len_val);
            default:                  return js_array_constructor(ctx, JS_UNDEFINED, 1, &len_val);
        }
    }
    ctor = JS_GetProperty(ctx, obj, JS_ATOM_constructor);
    if (JS_IsException(ctor))
        return ctor;
    if (JS_IsConstructor(ctx, ctor)) {
        /* legacy web compatibility */
        realm = JS_GetFunctionRealm(ctx, ctor);
        if (!realm) {
            JS_FreeValue(ctx, ctor);
            return JS_EXCEPTION;
        }
        if (realm != ctx &&
            js_same_value(ctx, ctor, realm->array_ctor)) {
            JS_FreeValue(ctx, ctor);
            ctor = JS_UNDEFINED;
        }
    }
    if (JS_IsObject(ctor)) {
        species = JS_GetProperty(ctx, ctor, JS_ATOM_Symbol_species);
        JS_FreeValue(ctx, ctor);
        if (JS_IsException(species))
            return species;
        ctor = species;
        if (JS_IsNull(ctor))
            ctor = JS_UNDEFINED;
    }
    if (JS_IsUndefined(ctor)) {
        goto use_default_constructor;
    } else {
        ret = JS_CallConstructor(ctx, ctor, 1, &len_val);
        JS_FreeValue(ctx, ctor);
        return ret;
    }
}

static const JSCFunctionListEntry js_array_funcs[] = {
    JS_CFUNC_DEF("isArray", 1, js_array_isArray ),
    JS_CFUNC_DEF("from", 1, js_array_from ),
    JS_CFUNC_DEF("of", 0, js_array_of ),
    JS_CGETSET_DEF("[Symbol.species]", js_get_this, NULL ),
};

static const JSCFunctionListEntry js_fast_array_funcs[] = {
    JS_CFUNC_DEF("isArray", 1, js_fast_array_isArray ),
    JS_CFUNC_DEF("from", 1, js_fast_array_from ),
    JS_CFUNC_DEF("of", 0, js_array_of ),
    JS_CGETSET_DEF("[Symbol.species]", js_get_this, NULL ),
};

static int JS_isConcatSpreadable(JSContext *ctx, JSValueConst obj)
{
    JSValue val;

    if (!JS_IsObject(obj))
        return FALSE;
    val = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_isConcatSpreadable);
    if (JS_IsException(val))
        return -1;
    if (!JS_IsUndefined(val))
        return JS_ToBoolFree(ctx, val);
    return JS_IsArray(ctx, obj);
}

static JSValue js_array_concat(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    JSValue obj, arr, val;
    int64_t len, n;
    int res;

    arr = JS_UNDEFINED;
    obj = JS_ToObject(ctx, this_val);
    if (JS_IsException(obj))
        goto exception;

    arr = JS_ArraySpeciesCreate(ctx, obj, JS_NewInt32(ctx, 0));
    if (JS_IsException(arr))
        goto exception;
    n = 0;
    for ( int i = -1; i < argc; i++ )
    {
        JSValueConst e = (i < 0) ? obj : argv[i];

        res = JS_isConcatSpreadable(ctx, e);
        if (res < 0)
            goto exception;
        if (res) {
            JSObject *parr = JS_VALUE_GET_OBJ(arr);
            JSObject *pe = JS_VALUE_GET_OBJ(e);
            if (js_get_length64(ctx, &len, e))
                goto exception;
            if (n + len > MAX_SAFE_INTEGER) {
array_too_long:
                JS_ThrowTypeError(ctx, "Array loo long");
                goto exception;
            }
            if ( parr->fast_array && pe->fast_array )
            {
                if ( expand_fast_array(ctx, parr, n + len) )
                    goto exception;
                for ( int k = 0; k < len; k++, n++ )
                {
                    set_value(ctx, &parr->u.array.u.values[n],
                              JS_DupValue(ctx, pe->u.array.u.values[k]));
                }
            }
            else
            {
                for ( int k = 0; k < len; k++, n++ )
                {
                    res = JS_TryGetPropertyInt64(ctx, e, k, &val);
                    if ( res < 0 )
                        goto exception;
                    if ( res ) {
                        if ( JS_DefinePropertyValueInt64(ctx, arr, n, val,
                                                         JS_PROP_C_W_E | JS_PROP_THROW) < 0 )
                            goto exception;
                    }
                }
            }
        } else {
            if ( n >= MAX_SAFE_INTEGER )
                goto array_too_long;
            if ( JS_DefinePropertyValueInt64(ctx, arr, n, JS_DupValue(ctx, e),
                                             JS_PROP_C_W_E | JS_PROP_THROW) < 0 )
                goto exception;
            n++;
        }
    }
    if (JS_SetProperty(ctx, arr, JS_ATOM_length, JS_NewInt64(ctx, n)) < 0)
        goto exception;

    JS_FreeValue(ctx, obj);
    return arr;

exception:
    JS_FreeValue(ctx, arr);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

JSValue js_array_every(JSContext *ctx, JSValueConst this_val,
                       int argc, JSValueConst *argv, int special)
{
    JSValue obj, val, index_val, res, ret;
    JSValueConst args[3];
    JSValueConst func, this_arg;
    int64_t len, k, n;
    int present;

    ret = JS_UNDEFINED;
    val = JS_UNDEFINED;
    if (special & special_TA) {
        obj = JS_DupValue(ctx, this_val);
        len = js_typed_array_get_length_internal(ctx, obj);
        if (len < 0)
            goto exception;
    } else {
        obj = JS_ToObject(ctx, this_val);
        if (js_get_length64(ctx, &len, obj))
            goto exception;
    }
    func = argv[0];
    this_arg = JS_UNDEFINED;
    if (argc > 1)
        this_arg = argv[1];

    if (check_function(ctx, func))
        goto exception;

    switch (special) {
    case special_every:
    case special_every | special_TA:
        ret = JS_TRUE;
        break;
    case special_some:
    case special_some | special_TA:
        ret = JS_FALSE;
        break;
    case special_map:
        /* XXX: JS_ArraySpeciesCreate should take int64_t */
        ret = JS_ArraySpeciesCreate(ctx, obj, JS_NewInt64(ctx, len));
        if (JS_IsException(ret))
            goto exception;
        break;
    case special_filter:
        ret = JS_ArraySpeciesCreate(ctx, obj, JS_NewInt32(ctx, 0));
        if (JS_IsException(ret))
            goto exception;
        break;
    case special_map | special_TA:
        args[0] = obj;
        args[1] = JS_NewInt32(ctx, len);
        ret = js_typed_array___speciesCreate(ctx, JS_UNDEFINED, 2, args);
        if (JS_IsException(ret))
            goto exception;
        break;
    case special_filter | special_TA:
        ret = JS_NewArray(ctx);
        if (JS_IsException(ret))
            goto exception;
        break;
    }
    n = 0;

    for(k = 0; k < len; k++) {
        if (special & special_TA) {
            val = JS_GetPropertyInt64(ctx, obj, k);
            if (JS_IsException(val))
                goto exception;
            present = TRUE;
        } else {
            present = JS_TryGetPropertyInt64(ctx, obj, k, &val);
            if (present < 0)
                goto exception;
        }
        if (present) {
            index_val = JS_NewInt64(ctx, k);
            if (JS_IsException(index_val))
                goto exception;
            args[0] = val;
            args[1] = index_val;
            args[2] = obj;
            res = JS_Call(ctx, func, this_arg, 3, args);
            JS_FreeValue(ctx, index_val);
            if (JS_IsException(res))
                goto exception;
            switch (special) {
            case special_every:
            case special_every | special_TA:
                if (!JS_ToBoolFree(ctx, res)) {
                    ret = JS_FALSE;
                    goto done;
                }
                break;
            case special_some:
            case special_some | special_TA:
                if (JS_ToBoolFree(ctx, res)) {
                    ret = JS_TRUE;
                    goto done;
                }
                break;
            case special_map:
                if (JS_DefinePropertyValueInt64(ctx, ret, k, res,
                                                JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                    goto exception;
                break;
            case special_map | special_TA:
                if (JS_SetPropertyValue(ctx, ret, JS_NewInt32(ctx, k), res, JS_PROP_THROW) < 0)
                    goto exception;
                break;
            case special_filter:
            case special_filter | special_TA:
                if (JS_ToBoolFree(ctx, res)) {
                    if (JS_DefinePropertyValueInt64(ctx, ret, n++, JS_DupValue(ctx, val),
                                                    JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                        goto exception;
                }
                break;
            default:
                JS_FreeValue(ctx, res);
                break;
            }
            JS_FreeValue(ctx, val);
            val = JS_UNDEFINED;
        }
    }
done:
    if (special == (special_filter | special_TA)) {
        JSValue arr;
        args[0] = obj;
        args[1] = JS_NewInt32(ctx, n);
        arr = js_typed_array___speciesCreate(ctx, JS_UNDEFINED, 2, args);
        if (JS_IsException(arr))
            goto exception;
        args[0] = ret;
        res = JS_Invoke(ctx, arr, JS_ATOM_set, 1, args);
        if (check_exception_free(ctx, res))
            goto exception;
        JS_FreeValue(ctx, ret);
        ret = arr;
    }
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, obj);
    return ret;

exception:
    JS_FreeValue(ctx, ret);
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

JSValue js_array_reduce(JSContext *ctx, JSValueConst this_val,
                        int argc, JSValueConst *argv, int special)
{
    JSValue obj, val, index_val, acc, acc1;
    JSValueConst args[4];
    JSValueConst func;
    int64_t len, k, k1;
    int present;

    acc = JS_UNDEFINED;
    val = JS_UNDEFINED;
    if (special & special_TA) {
        obj = JS_DupValue(ctx, this_val);
        len = js_typed_array_get_length_internal(ctx, obj);
        if (len < 0)
            goto exception;
    } else {
        obj = JS_ToObject(ctx, this_val);
        if (js_get_length64(ctx, &len, obj))
            goto exception;
    }
    func = argv[0];

    if (check_function(ctx, func))
        goto exception;

    k = 0;
    if (argc > 1) {
        acc = JS_DupValue(ctx, argv[1]);
    } else {
        for(;;) {
            if (k >= len) {
                JS_ThrowTypeError(ctx, "empty array");
                goto exception;
            }
            k1 = (special & special_reduceRight) ? len - k - 1 : k;
            k++;
            if (special & special_TA) {
                acc = JS_GetPropertyInt64(ctx, obj, k1);
                if (JS_IsException(acc))
                    goto exception;
                break;
            } else {
                present = JS_TryGetPropertyInt64(ctx, obj, k1, &acc);
                if (present < 0)
                    goto exception;
                if (present)
                    break;
            }
        }
    }
    for (; k < len; k++) {
        k1 = (special & special_reduceRight) ? len - k - 1 : k;
        if (special & special_TA) {
            val = JS_GetPropertyInt64(ctx, obj, k1);
            if (JS_IsException(val))
                goto exception;
            present = TRUE;
        } else {
            present = JS_TryGetPropertyInt64(ctx, obj, k1, &val);
            if (present < 0)
                goto exception;
        }
        if (present) {
            index_val = JS_NewInt64(ctx, k1);
            if (JS_IsException(index_val))
                goto exception;
            args[0] = acc;
            args[1] = val;
            args[2] = index_val;
            args[3] = obj;
            acc1 = JS_Call(ctx, func, JS_UNDEFINED, 4, args);
            JS_FreeValue(ctx, index_val);
            JS_FreeValue(ctx, val);
            val = JS_UNDEFINED;
            if (JS_IsException(acc1))
                goto exception;
            JS_FreeValue(ctx, acc);
            acc = acc1;
        }
    }
    JS_FreeValue(ctx, obj);
    return acc;

exception:
    JS_FreeValue(ctx, acc);
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_fill(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    JSValue obj;
    int64_t len, start, end;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    start = 0;
    if (argc > 1 && !JS_IsUndefined(argv[1])) {
        if (JS_ToInt64Clamp(ctx, &start, argv[1], 0, len, len))
            goto exception;
    }

    end = len;
    if (argc > 2 && !JS_IsUndefined(argv[2])) {
        if (JS_ToInt64Clamp(ctx, &end, argv[2], 0, len, len))
            goto exception;
    }

    /* XXX: should special case fast arrays */
    // FIXME fast array
    while (start < end) {
        if (JS_SetPropertyInt64(ctx, obj, start,
                                JS_DupValue(ctx, argv[0])) < 0)
            goto exception;
        start++;
    }
    return obj;

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

JSValue js_array_includes(JSContext *ctx, JSValueConst this_val,
                          int argc, JSValueConst *argv)
{
    JSValue obj, val;
    int64_t len, n, res;
    JSValue *arrp;
    uint32_t count;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    res = FALSE;
    if (len > 0) {
        n = 0;
        if (argc > 1) {
            if (JS_ToInt64Clamp(ctx, &n, argv[1], 0, len, len))
                goto exception;
        }
        if (js_get_fast_array(ctx, obj, &arrp, &count)) {
            for (; n < count; n++) {
                if (js_strict_eq2(ctx, JS_DupValue(ctx, argv[0]),
                                  JS_DupValue(ctx, arrp[n]),
                                  JS_EQ_SAME_VALUE_ZERO)) {
                    res = TRUE;
                    goto done;
                }
            }
        }
        for (; n < len; n++) {
            val = JS_GetPropertyInt64(ctx, obj, n);
            if (JS_IsException(val))
                goto exception;
            if (js_strict_eq2(ctx, JS_DupValue(ctx, argv[0]), val,
                              JS_EQ_SAME_VALUE_ZERO)) {
                res = TRUE;
                break;
            }
        }
    }
 done:
    JS_FreeValue(ctx, obj);
    return JS_NewBool(ctx, res);

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_indexOf(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValue obj, val;
    int64_t len, n, res;
    JSValue *arrp;
    uint32_t count;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    res = -1;
    if (len > 0) {
        n = 0;
        if (argc > 1) {
            if (JS_ToInt64Clamp(ctx, &n, argv[1], 0, len, len))
                goto exception;
        }
        if (js_get_fast_array(ctx, obj, &arrp, &count)) {
            for (; n < count; n++) {
                if (js_strict_eq2(ctx, JS_DupValue(ctx, argv[0]),
                                  JS_DupValue(ctx, arrp[n]), JS_EQ_STRICT)) {
                    res = n;
                    goto done;
                }
            }
        }
        for (; n < len; n++) {
            int present = JS_TryGetPropertyInt64(ctx, obj, n, &val);
            if (present < 0)
                goto exception;
            if (present) {
                if (js_strict_eq2(ctx, JS_DupValue(ctx, argv[0]), val, JS_EQ_STRICT)) {
                    res = n;
                    break;
                }
            }
        }
    }
 done:
    JS_FreeValue(ctx, obj);
    return JS_NewInt64(ctx, res);

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_lastIndexOf(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    JSValue obj, val;
    int64_t len, n, res;
    int present;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    res = -1;
    if (len > 0) {
        n = len - 1;
        if (argc > 1) {
            if (JS_ToInt64Clamp(ctx, &n, argv[1], -1, len - 1, len))
                goto exception;
        }
        /* XXX: should special case fast arrays */
        // FIXME fast array
        for (; n >= 0; n--) {
            present = JS_TryGetPropertyInt64(ctx, obj, n, &val);
            if (present < 0)
                goto exception;
            if (present) {
                if (js_strict_eq2(ctx, JS_DupValue(ctx, argv[0]), val, JS_EQ_STRICT)) {
                    res = n;
                    break;
                }
            }
        }
    }
    JS_FreeValue(ctx, obj);
    return JS_NewInt64(ctx, res);

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_find(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv, int findIndex)
{
    JSValueConst func, this_arg;
    JSValueConst args[3];
    JSValue obj, val, index_val, res;
    int64_t len, k;

    index_val = JS_UNDEFINED;
    val = JS_UNDEFINED;
    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    func = argv[0];
    if (check_function(ctx, func))
        goto exception;

    this_arg = JS_UNDEFINED;
    if (argc > 1)
        this_arg = argv[1];

    for(k = 0; k < len; k++) {
        index_val = JS_NewInt64(ctx, k);
        if (JS_IsException(index_val))
            goto exception;
        val = JS_GetPropertyValue(ctx, obj, index_val);
        if (JS_IsException(val))
            goto exception;
        args[0] = val;
        args[1] = index_val;
        args[2] = this_val;
        res = JS_Call(ctx, func, this_arg, 3, args);
        if (JS_IsException(res))
            goto exception;
        if (JS_ToBoolFree(ctx, res)) {
            if (findIndex) {
                JS_FreeValue(ctx, val);
                JS_FreeValue(ctx, obj);
                return index_val;
            } else {
                JS_FreeValue(ctx, index_val);
                JS_FreeValue(ctx, obj);
                return val;
            }
        }
        JS_FreeValue(ctx, val);
        JS_FreeValue(ctx, index_val);
    }
    JS_FreeValue(ctx, obj);
    if (findIndex)
        return JS_NewInt32(ctx, -1);
    else
        return JS_UNDEFINED;

exception:
    JS_FreeValue(ctx, index_val);
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_toString(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    JSValue obj, method, ret;

    obj = JS_ToObject(ctx, this_val);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    method = JS_GetProperty(ctx, obj, JS_ATOM_join);
    if (JS_IsException(method)) {
        ret = JS_EXCEPTION;
    } else
    if (!JS_IsFunction(ctx, method)) {
        /* Use intrinsic Object.prototype.toString */
        JS_FreeValue(ctx, method);
        ret = js_object_toString(ctx, obj, 0, NULL);
    } else {
        ret = JS_CallFree(ctx, method, obj, 0, NULL);
    }
    JS_FreeValue(ctx, obj);
    return ret;
}

static JSValue js_array_join(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv, int toLocaleString)
{
    JSValue obj, sep = JS_UNDEFINED, el;
    StringBuffer b_s, *b = &b_s;
    JSString *p = NULL;
    int64_t i, n;
    int c;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &n, obj))
        goto exception;

    c = ',';    /* default separator */
    if (!toLocaleString && argc > 0 && !JS_IsUndefined(argv[0])) {
        sep = JS_ToString(ctx, argv[0]);
        if (JS_IsException(sep))
            goto exception;
        p = JS_VALUE_GET_STRING(sep);
        if (p->len == 1 && !p->is_wide_char)
            c = p->u.str8[0];
        else
            c = -1;
    }
    string_buffer_init(ctx, b, 0);

    for(i = 0; i < n; i++) {
        if (i > 0) {
            if (c >= 0) {
                string_buffer_putc8(b, c);
            } else {
                string_buffer_concat(b, p, 0, p->len);
            }
        }
        el = JS_GetPropertyUint32(ctx, obj, i);
        if (JS_IsException(el))
            goto fail;
        if (!JS_IsNull(el) && !JS_IsUndefined(el)) {
            if (toLocaleString) {
                el = JS_ToLocaleStringFree(ctx, el);
            }
            if (string_buffer_concat_value_free(b, el))
                goto fail;
        }
    }
    JS_FreeValue(ctx, sep);
    JS_FreeValue(ctx, obj);
    return string_buffer_end(b);

fail:
    string_buffer_free(b);
    JS_FreeValue(ctx, sep);
exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

JSValue js_array_pop(JSContext *ctx, JSValueConst this_val,
                     int argc, JSValueConst *argv, int shift)
{
    JSValue obj, res = JS_UNDEFINED;
    int64_t len, newLen;
    JSValue *arrp;
    uint32_t count32;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;
    newLen = 0;
    if (len > 0) {
        newLen = len - 1;
        /* Special case fast arrays */
        if (js_get_fast_array(ctx, obj, &arrp, &count32)) {
            /* FIXME when can count32 not equal len? */
            if ( count32 != len )
                assert(0);
            JSObject *p = JS_VALUE_GET_OBJ(obj);
            if (shift) {
                res = arrp[0];
                memmove(arrp, arrp + 1, (count32 - 1) * sizeof(*arrp));
                p->u.array.count--;
            } else {
                res = arrp[count32 - 1];
                p->u.array.count--;
            }
        } else {
            if (shift) {
                res = JS_GetPropertyInt32(ctx, obj, 0);
                if (JS_IsException(res))
                    goto exception;
                if (JS_CopySubArray(ctx, obj, 0, 1, len - 1, +1))
                    goto exception;
            } else {
                res = JS_GetPropertyInt64(ctx, obj, newLen);
                if (JS_IsException(res))
                    goto exception;
            }
            if (JS_DeletePropertyInt64(ctx, obj, newLen, JS_PROP_THROW) < 0)
                goto exception;
        }
    }
    if (JS_SetProperty(ctx, obj, JS_ATOM_length, JS_NewInt64(ctx, newLen)) < 0)
        goto exception;

    JS_FreeValue(ctx, obj);
    return res;

 exception:
    JS_FreeValue(ctx, res);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

JSValue js_array_push(JSContext *ctx, JSValueConst this_val,
                      int argc, JSValueConst *argv, int unshift)
{
    JSValue obj;
    int i;
    int64_t len, from, newLen;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;
    newLen = len + argc;
    if (newLen > MAX_SAFE_INTEGER) {
        JS_ThrowTypeError(ctx, "Array loo long");
        goto exception;
    }
    from = len;
    if (unshift && argc > 0) {
        if (JS_CopySubArray(ctx, obj, argc, 0, len, -1))
            goto exception;
        from = 0;
    }
#if 0
    if ( p->fast_array )
    {
        uint32_t old_len = p->u.array.count;
        JSValue *arrp;
        if (expand_fast_array(ctx, p, old_len + argc))
            goto exception;
        arrp = p->u.array.u.values;
        for ( i = 0; i < argc; i++ )
            arrp[from + i] = JS_DupValue(ctx, argv[i]);
        p->u.array.count = old_len + argc;
        p->prop[0].u.value = JS_NewInt32(ctx, old_len + argc);
    }
    else
#endif
    {
        for(i = 0; i < argc; i++) {
            if (JS_SetPropertyInt64(ctx, obj, from + i,
                                    JS_DupValue(ctx, argv[i])) < 0)
                goto exception;
        }
    }
    if (JS_SetProperty(ctx, obj, JS_ATOM_length, JS_NewInt64(ctx, newLen)) < 0)
        goto exception;

    JS_FreeValue(ctx, obj);
    return JS_NewInt64(ctx, newLen);

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_reverse(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValue obj, lval, hval;
    JSValue *arrp;
    int64_t len, l, h;
    int l_present, h_present;
    uint32_t count32;

    lval = JS_UNDEFINED;
    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    /* Special case fast arrays */
    if (js_get_fast_array(ctx, obj, &arrp, &count32)) {
        /* FIXME when can count32 not equal len? */
        if ( count32 != len )
            assert(0);
        uint32_t ll, hh;

        if (count32 > 1) {
            for (ll = 0, hh = count32 - 1; ll < hh; ll++, hh--) {
                lval = arrp[ll];
                arrp[ll] = arrp[hh];
                arrp[hh] = lval;
            }
        }
        return obj;
    }

    for (l = 0, h = len - 1; l < h; l++, h--) {
        l_present = JS_TryGetPropertyInt64(ctx, obj, l, &lval);
        if (l_present < 0)
            goto exception;
        h_present = JS_TryGetPropertyInt64(ctx, obj, h, &hval);
        if (h_present < 0)
            goto exception;
        if (h_present) {
            if (JS_SetPropertyInt64(ctx, obj, l, hval) < 0)
                goto exception;

            if (l_present) {
                if (JS_SetPropertyInt64(ctx, obj, h, lval) < 0) {
                    lval = JS_UNDEFINED;
                    goto exception;
                }
                lval = JS_UNDEFINED;
            } else {
                if (JS_DeletePropertyInt64(ctx, obj, h, JS_PROP_THROW) < 0)
                    goto exception;
            }
        } else {
            if (l_present) {
                if (JS_DeletePropertyInt64(ctx, obj, l, JS_PROP_THROW) < 0)
                    goto exception;
                if (JS_SetPropertyInt64(ctx, obj, h, lval) < 0) {
                    lval = JS_UNDEFINED;
                    goto exception;
                }
                lval = JS_UNDEFINED;
            }
        }
    }
    return obj;

 exception:
    JS_FreeValue(ctx, lval);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_slice(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv, int splice)
{
    JSValue obj, arr, val, len_val;
    int64_t len, start, k, final, n, count, del_count, new_len;
    int kPresent;
    JSValue *arrp;
    uint32_t count32, i, item_count;

    arr = JS_UNDEFINED;
    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    if (JS_ToInt64Clamp(ctx, &start, argv[0], 0, len, len))
        goto exception;

    if (splice) {
        if (argc == 0) {
            item_count = 0;
            del_count = 0;
        } else
        if (argc == 1) {
            item_count = 0;
            del_count = len - start;
        } else {
            item_count = argc - 2;
            if (JS_ToInt64Clamp(ctx, &del_count, argv[1], 0, len - start, 0))
                goto exception;
        }
        if (len + item_count - del_count > MAX_SAFE_INTEGER) {
            JS_ThrowTypeError(ctx, "Array loo long");
            goto exception;
        }
        count = del_count;
    } else {
        item_count = 0; /* avoid warning */
        final = len;
        if (!JS_IsUndefined(argv[1])) {
            if (JS_ToInt64Clamp(ctx, &final, argv[1], 0, len, len))
                goto exception;
        }
        count = max_int64(final - start, 0);
    }
    len_val = JS_NewInt64(ctx, count);
    arr = JS_ArraySpeciesCreate(ctx, obj, len_val);
    JS_FreeValue(ctx, len_val);
    if (JS_IsException(arr))
        goto exception;

    k = start;
    final = start + count;
    n = 0;
    /* The fast array test on arr ensures that
       JS_CreateDataPropertyUint32() won't modify obj in case arr is
       an exotic object */
    /* Special case fast arrays */
    if (js_get_fast_array(ctx, obj, &arrp, &count32)) {
        // FIXME
        if ( js_is_fast_array(ctx, arr) )
        {
            /* XXX: should share code with fast array constructor */
            for (; k < final && k < count32; k++, n++) {
                if (JS_CreateDataPropertyUint32(ctx, arr, n, JS_DupValue(ctx, arrp[k]), JS_PROP_THROW) < 0)
                    goto exception;
            }
        }
    }
    /* Copy the remaining elements if any (handle case of inherited properties) */
    for (; k < final; k++, n++) {
        kPresent = JS_TryGetPropertyInt64(ctx, obj, k, &val);
        if (kPresent < 0)
            goto exception;
        if (kPresent) {
            if (JS_CreateDataPropertyUint32(ctx, arr, n, val, JS_PROP_THROW) < 0)
                goto exception;
        }
    }
    if (JS_SetProperty(ctx, arr, JS_ATOM_length, JS_NewInt64(ctx, n)) < 0)
        goto exception;

    if (splice) {
        new_len = len + item_count - del_count;
        if (item_count != del_count) {
            if (JS_CopySubArray(ctx, obj, start + item_count,
                                start + del_count, len - (start + del_count),
                                item_count <= del_count ? +1 : -1) < 0)
                goto exception;

            for (k = len; k-- > new_len; ) {
                if (JS_DeletePropertyInt64(ctx, obj, k, JS_PROP_THROW) < 0)
                    goto exception;
            }
        }
        for (i = 0; i < item_count; i++) {
            if (JS_SetPropertyInt64(ctx, obj, start + i, JS_DupValue(ctx, argv[i + 2])) < 0)
                goto exception;
        }
        if (JS_SetProperty(ctx, obj, JS_ATOM_length, JS_NewInt64(ctx, new_len)) < 0)
            goto exception;
    }
    JS_FreeValue(ctx, obj);
    return arr;

 exception:
    JS_FreeValue(ctx, obj);
    JS_FreeValue(ctx, arr);
    return JS_EXCEPTION;
}

static JSValue js_array_copyWithin(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    JSValue obj = JS_ToObject(ctx, this_val);
    int64_t len, from, to, final, count;
    JSObject *p;

    if (js_get_length64(ctx, &len, obj))
        goto exception;

    if (JS_ToInt64Clamp(ctx, &to, argv[0], 0, len, len))
        goto exception;

    if (JS_ToInt64Clamp(ctx, &from, argv[1], 0, len, len))
        goto exception;

    final = len;
    if (argc > 2 && !JS_IsUndefined(argv[2])) {
        if (JS_ToInt64Clamp(ctx, &final, argv[2], 0, len, len))
            goto exception;
    }

    count = min_int64(final - from, len - to);

    if (JS_CopySubArray(ctx, obj, to, from, count,
                        (from < to && to < from + count) ? -1 : +1))
        goto exception;

    return obj;

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static int64_t JS_FlattenIntoArray(JSContext *ctx, JSValueConst target,
                                   JSValueConst source, int64_t sourceLen,
                                   int64_t targetIndex, int depth,
                                   JSValueConst mapperFunction,
                                   JSValueConst thisArg)
{
    JSValue element;
    int64_t sourceIndex, elementLen;
    int present, is_array;

    if (js_check_stack_overflow(ctx->rt, 0)) {
        JS_ThrowStackOverflow(ctx);
        return -1;
    }

    for (sourceIndex = 0; sourceIndex < sourceLen; sourceIndex++) {
        present = JS_TryGetPropertyInt64(ctx, source, sourceIndex, &element);
        if (present < 0)
            return -1;
        if (!present)
            continue;
        if (!JS_IsUndefined(mapperFunction)) {
            JSValueConst args[3] = { element, JS_NewInt64(ctx, sourceIndex), source };
            element = JS_Call(ctx, mapperFunction, thisArg, 3, args);
            JS_FreeValue(ctx, (JSValue)args[0]);
            JS_FreeValue(ctx, (JSValue)args[1]);
            if (JS_IsException(element))
                return -1;
        }
        if (depth > 0) {
            is_array = JS_IsArray(ctx, element);
            if (is_array < 0)
                goto fail;
            if (is_array) {
                if (js_get_length64(ctx, &elementLen, element) < 0)
                    goto fail;
                targetIndex = JS_FlattenIntoArray(ctx, target, element,
                                                  elementLen, targetIndex,
                                                  depth - 1,
                                                  JS_UNDEFINED, JS_UNDEFINED);
                if (targetIndex < 0)
                    goto fail;
                JS_FreeValue(ctx, element);
                continue;
            }
        }
        if (targetIndex >= MAX_SAFE_INTEGER) {
            JS_ThrowTypeError(ctx, "Array too long");
            goto fail;
        }
        if (JS_DefinePropertyValueInt64(ctx, target, targetIndex, element,
                                        JS_PROP_C_W_E | JS_PROP_THROW) < 0)
            return -1;
        targetIndex++;
    }
    return targetIndex;

fail:
    JS_FreeValue(ctx, element);
    return -1;
}

static JSValue js_array_flatten(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv, int map)
{
    JSValue obj, arr;
    JSValueConst mapperFunction, thisArg;
    int64_t sourceLen;
    int depthNum;

    arr = JS_UNDEFINED;
    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &sourceLen, obj))
        goto exception;

    depthNum = 1;
    mapperFunction = JS_UNDEFINED;
    thisArg = JS_UNDEFINED;
    if (map) {
        mapperFunction = argv[0];
        if (argc > 1) {
            thisArg = argv[1];
        }
        if (check_function(ctx, mapperFunction))
            goto exception;
    } else {
        if (argc > 0 && !JS_IsUndefined(argv[0])) {
            if (JS_ToInt32Sat(ctx, &depthNum, argv[0]) < 0)
                goto exception;
        }
    }
    arr = JS_ArraySpeciesCreate(ctx, obj, JS_NewInt32(ctx, 0));
    if (JS_IsException(arr))
        goto exception;
    if (JS_FlattenIntoArray(ctx, arr, obj, sourceLen, 0, depthNum,
                            mapperFunction, thisArg) < 0)
        goto exception;
    JS_FreeValue(ctx, obj);
    return arr;

exception:
    JS_FreeValue(ctx, obj);
    JS_FreeValue(ctx, arr);
    return JS_EXCEPTION;
}

/* Array sort */

typedef struct ValueSlot {
    JSValue val;
    JSString *str;
    int64_t pos;
} ValueSlot;

struct array_sort_context {
    JSContext *ctx;
    int exception;
    int has_method;
    JSValueConst method;
};

static int js_array_cmp_generic(const void *a, const void *b, void *opaque) {
    struct array_sort_context *psc = opaque;
    JSContext *ctx = psc->ctx;
    JSValueConst argv[2];
    JSValue res;
    ValueSlot *ap = (ValueSlot *)(void *)a;
    ValueSlot *bp = (ValueSlot *)(void *)b;
    int cmp;

    if (psc->exception)
        return 0;

    if (psc->has_method) {
        /* custom sort function is specified as returning 0 for identical
         * objects: avoid method call overhead.
         */
        if (!memcmp(&ap->val, &bp->val, sizeof(ap->val)))
            goto cmp_same;
        argv[0] = ap->val;
        argv[1] = bp->val;
        res = JS_Call(ctx, psc->method, JS_UNDEFINED, 2, argv);
        if (JS_IsException(res))
            goto exception;
        if (JS_VALUE_GET_TAG(res) == JS_TAG_INT) {
            int val = JS_VALUE_GET_INT(res);
            cmp = (val > 0) - (val < 0);
        } else {
            double val;
            if (JS_ToFloat64Free(ctx, &val, res) < 0)
                goto exception;
            cmp = (val > 0) - (val < 0);
        }
    } else {
        /* Not supposed to bypass ToString even for identical objects as
         * tested in test262/test/built-ins/Array/prototype/sort/bug_596_1.js
         */
        if (!ap->str) {
            JSValue str = JS_ToString(ctx, ap->val);
            if (JS_IsException(str))
                goto exception;
            ap->str = JS_VALUE_GET_STRING(str);
        }
        if (!bp->str) {
            JSValue str = JS_ToString(ctx, bp->val);
            if (JS_IsException(str))
                goto exception;
            bp->str = JS_VALUE_GET_STRING(str);
        }
        cmp = js_string_compare(ctx, ap->str, bp->str);
    }
    if (cmp != 0)
        return cmp;
cmp_same:
    /* make sort stable: compare array offsets */
    return (ap->pos > bp->pos) - (ap->pos < bp->pos);

exception:
    psc->exception = 1;
    return 0;
}

static JSValue js_array_sort(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    struct array_sort_context asc = { ctx, 0, 0, argv[0] };
    JSValue obj = JS_UNDEFINED;
    ValueSlot *array = NULL;
    size_t array_size = 0, pos = 0, n = 0;
    int64_t i, len, undefined_count = 0;
    int present;

    if (!JS_IsUndefined(asc.method)) {
        if (check_function(ctx, asc.method))
            goto exception;
        asc.has_method = 1;
    }
    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    /* XXX: should special case fast arrays */
    // FIXME fast array
    for (i = 0; i < len; i++) {
        if (pos >= array_size) {
            size_t new_size, slack;
            ValueSlot *new_array;
            new_size = (array_size + (array_size >> 1) + 31) & ~15;
            new_array = js_realloc2(ctx, array, new_size * sizeof(*array), &slack);
            if (new_array == NULL)
                goto exception;
            new_size += slack / sizeof(*new_array);
            array = new_array;
            array_size = new_size;
        }
        present = JS_TryGetPropertyInt64(ctx, obj, i, &array[pos].val);
        if (present < 0)
            goto exception;
        if (present == 0)
            continue;
        if (JS_IsUndefined(array[pos].val)) {
            undefined_count++;
            continue;
        }
        array[pos].str = NULL;
        array[pos].pos = i;
        pos++;
    }
    rqsort(array, pos, sizeof(*array), js_array_cmp_generic, &asc);
    if (asc.exception)
        goto exception;

    /* XXX: should special case fast arrays */
    // FIXME fast array
    while (n < pos) {
        if (array[n].str)
            JS_FreeValue(ctx, JS_MKPTR(JS_TAG_STRING, array[n].str));
        if (array[n].pos == n) {
            JS_FreeValue(ctx, array[n].val);
        } else {
            if (JS_SetPropertyInt64(ctx, obj, n, array[n].val) < 0) {
                n++;
                goto exception;
            }
        }
        n++;
    }
    js_free(ctx, array);
    for (i = n; undefined_count-- > 0; i++) {
        if (JS_SetPropertyInt64(ctx, obj, i, JS_UNDEFINED) < 0)
            goto fail;
    }
    for (; i < len; i++) {
        if (JS_DeletePropertyInt64(ctx, obj, i, JS_PROP_THROW) < 0)
            goto fail;
    }
    return obj;

exception:
    for (; n < pos; n++) {
        JS_FreeValue(ctx, array[n].val);
        if (array[n].str)
            JS_FreeValue(ctx, JS_MKPTR(JS_TAG_STRING, array[n].str));
    }
    js_free(ctx, array);
fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

void js_array_iterator_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSArrayIteratorData *it = p->u.array_iterator_data;
    if (it) {
        JS_FreeValueRT(rt, it->obj);
        js_free_rt(rt, it);
    }
}

void js_array_iterator_mark(JSRuntime *rt, JSValueConst val,
                            JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSArrayIteratorData *it = p->u.array_iterator_data;
    if (it) {
        JS_MarkValue(rt, it->obj, mark_func);
    }
}

static JSValue js_create_array_internal(JSContext *ctx, int len, JSValueConst *tab, int class_id)
{
    JSValue obj;
    int i;

    if ( class_id == JS_CLASS_FAST_ARRAY )
        obj = JS_NewFastArray(ctx, NULL, 0);
    else
        obj = JS_NewArray(ctx);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    for(i = 0; i < len; i++) {
        if (JS_CreateDataPropertyUint32(ctx, obj, i, JS_DupValue(ctx, tab[i]), 0) < 0) {
            JS_FreeValue(ctx, obj);
            return JS_EXCEPTION;
        }
    }
    return obj;
}

JSValue js_create_array(JSContext *ctx, int len, JSValueConst *tab)
{
    return js_create_array_internal(ctx, len, tab, JS_CLASS_ARRAY);
}

JSValue js_create_fast_array(JSContext *ctx, int len, JSValueConst *tab)
{
    return js_create_array_internal(ctx, len, tab, JS_CLASS_FAST_ARRAY);
}

JSValue js_create_array_iterator(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int magic)
{
    JSValue enum_obj, arr;
    JSArrayIteratorData *it;
    JSIteratorKindEnum kind;
    int class_id;

    kind = magic & 3;
    if (magic & 4) {
        /* string iterator case */
        arr = JS_ToStringCheckObject(ctx, this_val);
        class_id = JS_CLASS_STRING_ITERATOR;
    } else {
        arr = JS_ToObject(ctx, this_val);
        class_id = JS_CLASS_ARRAY_ITERATOR;
    }
    if (JS_IsException(arr))
        goto fail;
    enum_obj = JS_NewObjectClass(ctx, class_id);
    if (JS_IsException(enum_obj))
        goto fail;
    it = js_malloc(ctx, sizeof(*it));
    if (!it)
        goto fail1;
    it->obj = arr;
    it->kind = kind;
    it->idx = 0;
    JS_SetOpaque(enum_obj, it);
    return enum_obj;
 fail1:
    JS_FreeValue(ctx, enum_obj);
 fail:
    JS_FreeValue(ctx, arr);
    return JS_EXCEPTION;
}

JSValue js_array_iterator_next(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv,
                               BOOL *pdone, int magic)
{
    JSArrayIteratorData *it;
    uint32_t len, idx;
    JSValue val, obj;
    JSObject *p;

    it = JS_GetOpaque2(ctx, this_val, JS_CLASS_ARRAY_ITERATOR);
    if (!it)
        goto fail1;
    if (JS_IsUndefined(it->obj))
        goto done;
    p = JS_VALUE_GET_OBJ(it->obj);
    if (p->class_id >= JS_CLASS_UINT8C_ARRAY &&
        p->class_id <= JS_CLASS_FLOAT64_ARRAY) {
        if (typed_array_is_detached(ctx, p)) {
            JS_ThrowTypeErrorDetachedArrayBuffer(ctx);
            goto fail1;
        }
        len = p->u.array.count;
    } else {
        if (js_get_length32(ctx, &len, it->obj)) {
        fail1:
            *pdone = FALSE;
            return JS_EXCEPTION;
        }
    }
    idx = it->idx;
    if (idx >= len) {
        JS_FreeValue(ctx, it->obj);
        it->obj = JS_UNDEFINED;
    done:
        *pdone = TRUE;
        return JS_UNDEFINED;
    }
    it->idx = idx + 1;
    *pdone = FALSE;
    if (it->kind == JS_ITERATOR_KIND_KEY) {
        return JS_NewUint32(ctx, idx);
    } else {
        val = JS_GetPropertyUint32(ctx, it->obj, idx);
        if (JS_IsException(val))
            return JS_EXCEPTION;
        if (it->kind == JS_ITERATOR_KIND_VALUE) {
            return val;
        } else {
            JSValueConst args[2];
            JSValue num;
            num = JS_NewUint32(ctx, idx);
            args[0] = num;
            args[1] = val;
            obj = js_create_array(ctx, 2, args);
            JS_FreeValue(ctx, val);
            JS_FreeValue(ctx, num);
            return obj;
        }
    }
}

JSValue js_iterator_proto_iterator(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    return JS_DupValue(ctx, this_val);
}

static const JSCFunctionListEntry js_iterator_proto_funcs[] = {
    JS_CFUNC_DEF("[Symbol.iterator]", 0, js_iterator_proto_iterator ),
};

static const JSCFunctionListEntry js_array_proto_funcs[] = {
    JS_CFUNC_DEF("concat", 1, js_array_concat ),
    JS_CFUNC_MAGIC_DEF("every", 1, js_array_every, special_every ),
    JS_CFUNC_MAGIC_DEF("some", 1, js_array_every, special_some ),
    JS_CFUNC_MAGIC_DEF("forEach", 1, js_array_every, special_forEach ),
    JS_CFUNC_MAGIC_DEF("map", 1, js_array_every, special_map ),
    JS_CFUNC_MAGIC_DEF("filter", 1, js_array_every, special_filter ),
    JS_CFUNC_MAGIC_DEF("reduce", 1, js_array_reduce, special_reduce ),
    JS_CFUNC_MAGIC_DEF("reduceRight", 1, js_array_reduce, special_reduceRight ),
    JS_CFUNC_DEF("fill", 1, js_array_fill ),
    JS_CFUNC_MAGIC_DEF("find", 1, js_array_find, 0 ),
    JS_CFUNC_MAGIC_DEF("findIndex", 1, js_array_find, 1 ),
    JS_CFUNC_DEF("indexOf", 1, js_array_indexOf ),
    JS_CFUNC_DEF("lastIndexOf", 1, js_array_lastIndexOf ),
    JS_CFUNC_DEF("includes", 1, js_array_includes ),
    JS_CFUNC_MAGIC_DEF("join", 1, js_array_join, 0 ),
    JS_CFUNC_DEF("toString", 0, js_array_toString ),
    JS_CFUNC_MAGIC_DEF("toLocaleString", 0, js_array_join, 1 ),
    JS_CFUNC_MAGIC_DEF("pop", 0, js_array_pop, 0 ),
    JS_CFUNC_MAGIC_DEF("push", 1, js_array_push, 0 ),
    JS_CFUNC_MAGIC_DEF("shift", 0, js_array_pop, 1 ),
    JS_CFUNC_MAGIC_DEF("unshift", 1, js_array_push, 1 ),
    JS_CFUNC_DEF("reverse", 0, js_array_reverse ),
    JS_CFUNC_DEF("sort", 1, js_array_sort ),
    JS_CFUNC_MAGIC_DEF("slice", 2, js_array_slice, 0 ),
    JS_CFUNC_MAGIC_DEF("splice", 2, js_array_slice, 1 ),
    JS_CFUNC_DEF("copyWithin", 2, js_array_copyWithin ),
    JS_CFUNC_MAGIC_DEF("flatMap", 1, js_array_flatten, 1 ),
    JS_CFUNC_MAGIC_DEF("flat", 0, js_array_flatten, 0 ),
    JS_CFUNC_MAGIC_DEF("values", 0, js_create_array_iterator, JS_ITERATOR_KIND_VALUE ),
    JS_ALIAS_DEF("[Symbol.iterator]", "values" ),
    JS_CFUNC_MAGIC_DEF("keys", 0, js_create_array_iterator, JS_ITERATOR_KIND_KEY ),
    JS_CFUNC_MAGIC_DEF("entries", 0, js_create_array_iterator, JS_ITERATOR_KIND_KEY_AND_VALUE ),
};

static const JSCFunctionListEntry js_array_iterator_proto_funcs[] = {
    JS_ITERATOR_NEXT_DEF("next", 0, js_array_iterator_next, 0 ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Array Iterator", JS_PROP_CONFIGURABLE ),
};

void JS_AddIntrinsicBasicArray(JSContext *ctx)
{
    /* the array prototype is an array */
    ctx->class_proto[JS_CLASS_ARRAY] =
        JS_NewObjectProtoClass2(ctx, ctx->class_proto[JS_CLASS_OBJECT],
                                JS_CLASS_ARRAY);

    ctx->array_shape = js_new_shape2(ctx, get_proto_obj(ctx->class_proto[JS_CLASS_ARRAY]),
                                     JS_PROP_INITIAL_HASH_SIZE, 1);
    add_shape_property(ctx, &ctx->array_shape, NULL,
                       JS_ATOM_length, JS_PROP_WRITABLE | JS_PROP_LENGTH);

    /* the fast_array prototype is an array */
    ctx->class_proto[JS_CLASS_FAST_ARRAY] =
        JS_NewObjectProtoClass2(ctx, ctx->class_proto[JS_CLASS_OBJECT],
                                JS_CLASS_FAST_ARRAY);

    ctx->fast_array_shape = js_new_shape2(ctx, get_proto_obj(ctx->class_proto[JS_CLASS_FAST_ARRAY]),
                                          JS_PROP_INITIAL_HASH_SIZE, 1);
    add_shape_property(ctx, &ctx->fast_array_shape, NULL,
                       JS_ATOM_length, JS_PROP_WRITABLE | JS_PROP_LENGTH);
}

void JS_AddIntrinsicArray(JSContext *ctx)
{
    JSValueConst obj;
    JSValue obj1;

    /* Iterator prototype */
    ctx->iterator_proto = JS_NewObject(ctx);
    JS_SetPropertyFunctionList(ctx, ctx->iterator_proto,
                               js_iterator_proto_funcs,
                               countof(js_iterator_proto_funcs));

    /* Array */
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_ARRAY],
                               js_array_proto_funcs,
                               countof(js_array_proto_funcs));

    obj = JS_NewGlobalCConstructor(ctx, "Array", js_array_constructor, 1,
                                   ctx->class_proto[JS_CLASS_ARRAY]);
    ctx->array_ctor = JS_DupValue(ctx, obj);
    JS_SetPropertyFunctionList(ctx, obj, js_array_funcs,
                               countof(js_array_funcs));

    /* FastArray */
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_FAST_ARRAY],
                               js_array_proto_funcs,
                               countof(js_array_proto_funcs));

    obj = JS_NewGlobalCConstructor(ctx, "FastArray", js_fast_array_constructor, 1,
                                   ctx->class_proto[JS_CLASS_FAST_ARRAY]);
    ctx->fast_array_ctor = JS_DupValue(ctx, obj);
    JS_SetPropertyFunctionList(ctx, obj, js_fast_array_funcs,
                               countof(js_fast_array_funcs));

    /* XXX: create auto_initializer */
    {
        /* initialize Array.prototype[Symbol.unscopables] */
        char const unscopables[] = "copyWithin" "\0" "entries" "\0" "fill" "\0" "find" "\0"
            "findIndex" "\0" "flat" "\0" "flatMap" "\0" "includes" "\0" "keys" "\0" "values" "\0";
        const char *p = unscopables;
        obj1 = JS_NewObjectProto(ctx, JS_NULL);
        for(p = unscopables; *p; p += strlen(p) + 1) {
            JS_DefinePropertyValueStr(ctx, obj1, p, JS_TRUE, JS_PROP_C_W_E);
        }
        JS_DefinePropertyValue(ctx, ctx->class_proto[JS_CLASS_ARRAY],
                               JS_ATOM_Symbol_unscopables, obj1,
                               JS_PROP_CONFIGURABLE);
        JS_DefinePropertyValue(ctx, ctx->class_proto[JS_CLASS_FAST_ARRAY],
                               JS_ATOM_Symbol_unscopables, JS_DupValue(ctx, obj1),
                               JS_PROP_CONFIGURABLE);
    }

    /* needed to initialize arguments[Symbol.iterator] */
    ctx->array_proto_values =
        JS_GetProperty(ctx, ctx->class_proto[JS_CLASS_ARRAY], JS_ATOM_values);

    ctx->class_proto[JS_CLASS_ARRAY_ITERATOR] = JS_NewObjectProto(ctx, ctx->iterator_proto);
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_ARRAY_ITERATOR],
                               js_array_iterator_proto_funcs,
                               countof(js_array_iterator_proto_funcs));
}
