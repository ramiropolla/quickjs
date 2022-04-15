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

/* Number */

static JSValue js_number_constructor(JSContext *ctx, JSValueConst new_target,
                                     int argc, JSValueConst *argv)
{
    JSValue val, obj;
    if (argc == 0) {
        val = JS_NewInt32(ctx, 0);
    } else {
        val = JS_ToNumeric(ctx, argv[0]);
        if (JS_IsException(val))
            return val;
        switch(JS_VALUE_GET_TAG(val)) {
        case JS_TAG_BIG_INT:
        case JS_TAG_BIG_FLOAT:
            {
                JSBigFloat *p = JS_VALUE_GET_PTR(val);
                double d;
                bf_get_float64(&p->num, &d, BF_RNDN);
                JS_FreeValue(ctx, val);
                val = __JS_NewFloat64(ctx, d);
            }
            break;
        case JS_TAG_BIG_DECIMAL:
            val = JS_ToStringFree(ctx, val);
            if (JS_IsException(val))
                return val;
            val = JS_ToNumberFree(ctx, val);
            if (JS_IsException(val))
                return val;
            break;
        default:
            break;
        }
    }
    if (!JS_IsUndefined(new_target)) {
        obj = js_create_from_ctor(ctx, new_target, JS_CLASS_NUMBER);
        if (!JS_IsException(obj))
            JS_SetObjectData(ctx, obj, val);
        return obj;
    } else {
        return val;
    }
}

static JSValue js_number_isNaN(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    if (!JS_IsNumber(argv[0]))
        return JS_FALSE;
    return js_global_isNaN(ctx, this_val, argc, argv);
}

static JSValue js_number_isFinite(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    if (!JS_IsNumber(argv[0]))
        return JS_FALSE;
    return js_global_isFinite(ctx, this_val, argc, argv);
}

static JSValue js_number_isInteger(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    int ret;
    ret = JS_NumberIsInteger(ctx, argv[0]);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_number_isSafeInteger(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    double d;
    if (!JS_IsNumber(argv[0]))
        return JS_FALSE;
    if (unlikely(JS_ToFloat64(ctx, &d, argv[0])))
        return JS_EXCEPTION;
    return JS_NewBool(ctx, is_safe_integer(d));
}

static const JSCFunctionListEntry js_number_funcs[] = {
    /* global ParseInt and parseFloat should be defined already or delayed */
    JS_ALIAS_BASE_DEF("parseInt", "parseInt", 0 ),
    JS_ALIAS_BASE_DEF("parseFloat", "parseFloat", 0 ),
    JS_CFUNC_DEF("isNaN", 1, js_number_isNaN ),
    JS_CFUNC_DEF("isFinite", 1, js_number_isFinite ),
    JS_CFUNC_DEF("isInteger", 1, js_number_isInteger ),
    JS_CFUNC_DEF("isSafeInteger", 1, js_number_isSafeInteger ),
    JS_PROP_DOUBLE_DEF("MAX_VALUE", 1.7976931348623157e+308, 0 ),
    JS_PROP_DOUBLE_DEF("MIN_VALUE", 5e-324, 0 ),
    JS_PROP_DOUBLE_DEF("NaN", NAN, 0 ),
    JS_PROP_DOUBLE_DEF("NEGATIVE_INFINITY", -INFINITY, 0 ),
    JS_PROP_DOUBLE_DEF("POSITIVE_INFINITY", INFINITY, 0 ),
    JS_PROP_DOUBLE_DEF("EPSILON", 2.220446049250313e-16, 0 ), /* ES6 */
    JS_PROP_DOUBLE_DEF("MAX_SAFE_INTEGER", 9007199254740991.0, 0 ), /* ES6 */
    JS_PROP_DOUBLE_DEF("MIN_SAFE_INTEGER", -9007199254740991.0, 0 ), /* ES6 */
};

static JSValue js_thisNumberValue(JSContext *ctx, JSValueConst this_val)
{
    if (JS_IsNumber(this_val))
        return JS_DupValue(ctx, this_val);

    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(this_val);
        if (p->class_id == JS_CLASS_NUMBER) {
            if (JS_IsNumber(p->u.object_data))
                return JS_DupValue(ctx, p->u.object_data);
        }
    }
    return JS_ThrowTypeError(ctx, "not a number");
}

static JSValue js_number_valueOf(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    return js_thisNumberValue(ctx, this_val);
}

int js_get_radix(JSContext *ctx, JSValueConst val)
{
    int radix;
    if (JS_ToInt32Sat(ctx, &radix, val))
        return -1;
    if (radix < 2 || radix > 36) {
        JS_ThrowRangeError(ctx, "radix must be between 2 and 36");
        return -1;
    }
    return radix;
}

static JSValue js_number_toString(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv, int magic)
{
    JSValue val;
    int base;
    double d;

    val = js_thisNumberValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (magic || JS_IsUndefined(argv[0])) {
        base = 10;
    } else {
        base = js_get_radix(ctx, argv[0]);
        if (base < 0)
            goto fail;
    }
    if (JS_ToFloat64Free(ctx, &d, val))
        return JS_EXCEPTION;
    return js_dtoa(ctx, d, base, 0, JS_DTOA_VAR_FORMAT);
 fail:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static JSValue js_number_toFixed(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    JSValue val;
    int f;
    double d;

    val = js_thisNumberValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (JS_ToFloat64Free(ctx, &d, val))
        return JS_EXCEPTION;
    if (JS_ToInt32Sat(ctx, &f, argv[0]))
        return JS_EXCEPTION;
    if (f < 0 || f > 100)
        return JS_ThrowRangeError(ctx, "invalid number of digits");
    if (fabs(d) >= 1e21) {
        return JS_ToStringFree(ctx, __JS_NewFloat64(ctx, d));
    } else {
        return js_dtoa(ctx, d, 10, f, JS_DTOA_FRAC_FORMAT);
    }
}

static JSValue js_number_toExponential(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    JSValue val;
    int f, flags;
    double d;

    val = js_thisNumberValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (JS_ToFloat64Free(ctx, &d, val))
        return JS_EXCEPTION;
    if (JS_ToInt32Sat(ctx, &f, argv[0]))
        return JS_EXCEPTION;
    if (!isfinite(d)) {
        return JS_ToStringFree(ctx,  __JS_NewFloat64(ctx, d));
    }
    if (JS_IsUndefined(argv[0])) {
        flags = 0;
        f = 0;
    } else {
        if (f < 0 || f > 100)
            return JS_ThrowRangeError(ctx, "invalid number of digits");
        f++;
        flags = JS_DTOA_FIXED_FORMAT;
    }
    return js_dtoa(ctx, d, 10, f, flags | JS_DTOA_FORCE_EXP);
}

static JSValue js_number_toPrecision(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv)
{
    JSValue val;
    int p;
    double d;

    val = js_thisNumberValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (JS_ToFloat64Free(ctx, &d, val))
        return JS_EXCEPTION;
    if (JS_IsUndefined(argv[0]))
        goto to_string;
    if (JS_ToInt32Sat(ctx, &p, argv[0]))
        return JS_EXCEPTION;
    if (!isfinite(d)) {
    to_string:
        return JS_ToStringFree(ctx,  __JS_NewFloat64(ctx, d));
    }
    if (p < 1 || p > 100)
        return JS_ThrowRangeError(ctx, "invalid number of digits");
    return js_dtoa(ctx, d, 10, p, JS_DTOA_FIXED_FORMAT);
}

static const JSCFunctionListEntry js_number_proto_funcs[] = {
    JS_CFUNC_DEF("toExponential", 1, js_number_toExponential ),
    JS_CFUNC_DEF("toFixed", 1, js_number_toFixed ),
    JS_CFUNC_DEF("toPrecision", 1, js_number_toPrecision ),
    JS_CFUNC_MAGIC_DEF("toString", 1, js_number_toString, 0 ),
    JS_CFUNC_MAGIC_DEF("toLocaleString", 0, js_number_toString, 1 ),
    JS_CFUNC_DEF("valueOf", 0, js_number_valueOf ),
};

JSValue js_parseInt(JSContext *ctx, JSValueConst this_val,
                    int argc, JSValueConst *argv)
{
    const char *str, *p;
    int radix, flags;
    JSValue ret;

    str = JS_ToCString(ctx, argv[0]);
    if (!str)
        return JS_EXCEPTION;
    if (JS_ToInt32(ctx, &radix, argv[1])) {
        JS_FreeCString(ctx, str);
        return JS_EXCEPTION;
    }
    if (radix != 0 && (radix < 2 || radix > 36)) {
        ret = JS_NAN;
    } else {
        p = str;
        p += skip_spaces(p);
        flags = ATOD_INT_ONLY | ATOD_ACCEPT_PREFIX_AFTER_SIGN;
        ret = js_atof(ctx, p, NULL, radix, flags);
    }
    JS_FreeCString(ctx, str);
    return ret;
}

JSValue js_parseFloat(JSContext *ctx, JSValueConst this_val,
                      int argc, JSValueConst *argv)
{
    const char *str, *p;
    JSValue ret;

    str = JS_ToCString(ctx, argv[0]);
    if (!str)
        return JS_EXCEPTION;
    p = str;
    p += skip_spaces(p);
    ret = js_atof(ctx, p, NULL, 10, 0);
    JS_FreeCString(ctx, str);
    return ret;
}

void JS_AddIntrinsicNumber(JSContext *ctx)
{
    JSValueConst number_obj;

    /* Number */
    ctx->class_proto[JS_CLASS_NUMBER] = JS_NewObjectProtoClass2(ctx, ctx->class_proto[JS_CLASS_OBJECT],
                                                                JS_CLASS_NUMBER);
    JS_SetObjectData(ctx, ctx->class_proto[JS_CLASS_NUMBER], JS_NewInt32(ctx, 0));
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_NUMBER],
                               js_number_proto_funcs,
                               countof(js_number_proto_funcs));
    number_obj = JS_NewGlobalCConstructor(ctx, "Number", js_number_constructor, 1,
                                          ctx->class_proto[JS_CLASS_NUMBER]);
    JS_SetPropertyFunctionList(ctx, number_obj, js_number_funcs, countof(js_number_funcs));
}
