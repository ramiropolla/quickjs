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

/* 2 <= base <= 36 */
char *i64toa(char *buf_end, int64_t n, unsigned int base)
{
    char *q = buf_end;
    int digit, is_neg;

    is_neg = 0;
    if (n < 0) {
        is_neg = 1;
        n = -n;
    }
    *--q = '\0';
    do {
        digit = (uint64_t)n % base;
        n = (uint64_t)n / base;
        if (digit < 10)
            digit += '0';
        else
            digit += 'a' - 10;
        *--q = digit;
    } while (n != 0);
    if (is_neg)
        *--q = '-';
    return q;
}

/* buf1 contains the printf result */
void js_ecvt1(double d, int n_digits, int *decpt, int *sign, char *buf,
              int rounding_mode, char *buf1, int buf1_size)
{
    if (rounding_mode != FE_TONEAREST)
        fesetround(rounding_mode);
    snprintf(buf1, buf1_size, "%+.*e", n_digits - 1, d);
    if (rounding_mode != FE_TONEAREST)
        fesetround(FE_TONEAREST);
    *sign = (buf1[0] == '-');
    /* mantissa */
    buf[0] = buf1[1];
    if (n_digits > 1)
        memcpy(buf + 1, buf1 + 3, n_digits - 1);
    buf[n_digits] = '\0';
    /* exponent */
    *decpt = atoi(buf1 + n_digits + 2 + (n_digits > 1)) + 1;
}

/* maximum buffer size for js_dtoa */
#define JS_DTOA_BUF_SIZE 128

/* needed because ecvt usually limits the number of digits to
   17. Return the number of digits. */
int js_ecvt(double d, int n_digits, int *decpt, int *sign, char *buf,
            BOOL is_fixed)
{
    int rounding_mode;
    char buf_tmp[JS_DTOA_BUF_SIZE];

    if (!is_fixed) {
        unsigned int n_digits_min, n_digits_max;
        /* find the minimum amount of digits (XXX: inefficient but simple) */
        n_digits_min = 1;
        n_digits_max = 17;
        while (n_digits_min < n_digits_max) {
            n_digits = (n_digits_min + n_digits_max) / 2;
            js_ecvt1(d, n_digits, decpt, sign, buf, FE_TONEAREST,
                     buf_tmp, sizeof(buf_tmp));
            if (strtod(buf_tmp, NULL) == d) {
                /* no need to keep the trailing zeros */
                while (n_digits >= 2 && buf[n_digits - 1] == '0')
                    n_digits--;
                n_digits_max = n_digits;
            } else {
                n_digits_min = n_digits + 1;
            }
        }
        n_digits = n_digits_max;
        rounding_mode = FE_TONEAREST;
    } else {
        rounding_mode = FE_TONEAREST;
#ifdef CONFIG_PRINTF_RNDN
        {
            char buf1[JS_DTOA_BUF_SIZE], buf2[JS_DTOA_BUF_SIZE];
            int decpt1, sign1, decpt2, sign2;
            /* The JS rounding is specified as round to nearest ties away
               from zero (RNDNA), but in printf the "ties" case is not
               specified (for example it is RNDN for glibc, RNDNA for
               Windows), so we must round manually. */
            js_ecvt1(d, n_digits + 1, &decpt1, &sign1, buf1, FE_TONEAREST,
                     buf_tmp, sizeof(buf_tmp));
            /* XXX: could use 2 digits to reduce the average running time */
            if (buf1[n_digits] == '5') {
                js_ecvt1(d, n_digits + 1, &decpt1, &sign1, buf1, FE_DOWNWARD,
                         buf_tmp, sizeof(buf_tmp));
                js_ecvt1(d, n_digits + 1, &decpt2, &sign2, buf2, FE_UPWARD,
                         buf_tmp, sizeof(buf_tmp));
                if (memcmp(buf1, buf2, n_digits + 1) == 0 && decpt1 == decpt2) {
                    /* exact result: round away from zero */
                    if (sign1)
                        rounding_mode = FE_DOWNWARD;
                    else
                        rounding_mode = FE_UPWARD;
                }
            }
        }
#endif /* CONFIG_PRINTF_RNDN */
    }
    js_ecvt1(d, n_digits, decpt, sign, buf, rounding_mode,
             buf_tmp, sizeof(buf_tmp));
    return n_digits;
}

int js_fcvt1(char *buf, int buf_size, double d, int n_digits,
             int rounding_mode)
{
    int n;
    if (rounding_mode != FE_TONEAREST)
        fesetround(rounding_mode);
    n = snprintf(buf, buf_size, "%.*f", n_digits, d);
    if (rounding_mode != FE_TONEAREST)
        fesetround(FE_TONEAREST);
    assert(n < buf_size);
    return n;
}

void js_fcvt(char *buf, int buf_size, double d, int n_digits)
{
    int rounding_mode;
    rounding_mode = FE_TONEAREST;
#ifdef CONFIG_PRINTF_RNDN
    {
        int n1, n2;
        char buf1[JS_DTOA_BUF_SIZE];
        char buf2[JS_DTOA_BUF_SIZE];

        /* The JS rounding is specified as round to nearest ties away from
           zero (RNDNA), but in printf the "ties" case is not specified
           (for example it is RNDN for glibc, RNDNA for Windows), so we
           must round manually. */
        n1 = js_fcvt1(buf1, sizeof(buf1), d, n_digits + 1, FE_TONEAREST);
        rounding_mode = FE_TONEAREST;
        /* XXX: could use 2 digits to reduce the average running time */
        if (buf1[n1 - 1] == '5') {
            n1 = js_fcvt1(buf1, sizeof(buf1), d, n_digits + 1, FE_DOWNWARD);
            n2 = js_fcvt1(buf2, sizeof(buf2), d, n_digits + 1, FE_UPWARD);
            if (n1 == n2 && memcmp(buf1, buf2, n1) == 0) {
                /* exact result: round away from zero */
                if (buf1[0] == '-')
                    rounding_mode = FE_DOWNWARD;
                else
                    rounding_mode = FE_UPWARD;
            }
        }
    }
#endif /* CONFIG_PRINTF_RNDN */
    js_fcvt1(buf, buf_size, d, n_digits, rounding_mode);
}

/* XXX: slow and maybe not fully correct. Use libbf when it is fast enough.
   XXX: radix != 10 is only supported for small integers
*/
void js_dtoa1(char *buf, double d, int radix, int n_digits, int flags)
{
    char *q;

    if (!isfinite(d)) {
        if (isnan(d)) {
            strcpy(buf, "NaN");
        } else {
            q = buf;
            if (d < 0)
                *q++ = '-';
            strcpy(q, "Infinity");
        }
    } else if (flags == JS_DTOA_VAR_FORMAT) {
        int64_t i64;
        char buf1[70], *ptr;
        i64 = (int64_t)d;
        if (d != i64 || i64 > MAX_SAFE_INTEGER || i64 < -MAX_SAFE_INTEGER)
            goto generic_conv;
        /* fast path for integers */
        ptr = i64toa(buf1 + sizeof(buf1), i64, radix);
        strcpy(buf, ptr);
    } else {
        if (d == 0.0)
            d = 0.0; /* convert -0 to 0 */
        if (flags == JS_DTOA_FRAC_FORMAT) {
            js_fcvt(buf, JS_DTOA_BUF_SIZE, d, n_digits);
        } else {
            char buf1[JS_DTOA_BUF_SIZE];
            int sign, decpt, k, n, i, p, n_max;
            BOOL is_fixed;
        generic_conv:
            is_fixed = ((flags & 3) == JS_DTOA_FIXED_FORMAT);
            if (is_fixed) {
                n_max = n_digits;
            } else {
                n_max = 21;
            }
            /* the number has k digits (k >= 1) */
            k = js_ecvt(d, n_digits, &decpt, &sign, buf1, is_fixed);
            n = decpt; /* d=10^(n-k)*(buf1) i.e. d= < x.yyyy 10^(n-1) */
            q = buf;
            if (sign)
                *q++ = '-';
            if (flags & JS_DTOA_FORCE_EXP)
                goto force_exp;
            if (n >= 1 && n <= n_max) {
                if (k <= n) {
                    memcpy(q, buf1, k);
                    q += k;
                    for(i = 0; i < (n - k); i++)
                        *q++ = '0';
                    *q = '\0';
                } else {
                    /* k > n */
                    memcpy(q, buf1, n);
                    q += n;
                    *q++ = '.';
                    for(i = 0; i < (k - n); i++)
                        *q++ = buf1[n + i];
                    *q = '\0';
                }
            } else if (n >= -5 && n <= 0) {
                *q++ = '0';
                *q++ = '.';
                for(i = 0; i < -n; i++)
                    *q++ = '0';
                memcpy(q, buf1, k);
                q += k;
                *q = '\0';
            } else {
            force_exp:
                /* exponential notation */
                *q++ = buf1[0];
                if (k > 1) {
                    *q++ = '.';
                    for(i = 1; i < k; i++)
                        *q++ = buf1[i];
                }
                *q++ = 'e';
                p = n - 1;
                if (p >= 0)
                    *q++ = '+';
                sprintf(q, "%d", p);
            }
        }
    }
}

JSValue js_dtoa(JSContext *ctx,
                double d, int radix, int n_digits, int flags)
{
    char buf[JS_DTOA_BUF_SIZE];
    js_dtoa1(buf, d, radix, n_digits, flags);
    return JS_NewString(ctx, buf);
}

/* XXX: remove */
static double js_strtod(const char *p, int radix, BOOL is_float)
{
    double d;
    int c;

    if (!is_float || radix != 10) {
        uint64_t n_max, n;
        int int_exp, is_neg;

        is_neg = 0;
        if (*p == '-') {
            is_neg = 1;
            p++;
        }

        /* skip leading zeros */
        while (*p == '0')
            p++;
        n = 0;
        if (radix == 10)
            n_max = ((uint64_t)-1 - 9) / 10; /* most common case */
        else
            n_max = ((uint64_t)-1 - (radix - 1)) / radix;
        /* XXX: could be more precise */
        int_exp = 0;
        while (*p != '\0') {
            c = to_digit((uint8_t)*p);
            if (c >= radix)
                break;
            if (n <= n_max) {
                n = n * radix + c;
            } else {
                int_exp++;
            }
            p++;
        }
        d = n;
        if (int_exp != 0) {
            d *= pow(radix, int_exp);
        }
        if (is_neg)
            d = -d;
    } else {
        d = strtod(p, NULL);
    }
    return d;
}

/* return an exception in case of memory error. Return JS_NAN if
   invalid syntax */
JSValue js_atof2(JSContext *ctx, const char *str, const char **pp,
                 int radix, int flags, slimb_t *pexponent)
{
    const char *p, *p_start;
    int sep, is_neg;
    BOOL is_float, has_legacy_octal;
    int atod_type = flags & ATOD_TYPE_MASK;
    char buf1[64], *buf;
    int i, j, len;
    BOOL buf_allocated = FALSE;
    JSValue val;

    /* optional separator between digits */
    sep = (flags & ATOD_ACCEPT_UNDERSCORES) ? '_' : 256;
    has_legacy_octal = FALSE;

    p = str;
    p_start = p;
    is_neg = 0;
    if (p[0] == '+') {
        p++;
        p_start++;
        if (!(flags & ATOD_ACCEPT_PREFIX_AFTER_SIGN))
            goto no_radix_prefix;
    } else if (p[0] == '-') {
        p++;
        p_start++;
        is_neg = 1;
        if (!(flags & ATOD_ACCEPT_PREFIX_AFTER_SIGN))
            goto no_radix_prefix;
    }
    if (p[0] == '0') {
        if ((p[1] == 'x' || p[1] == 'X') &&
            (radix == 0 || radix == 16)) {
            p += 2;
            radix = 16;
        } else if ((p[1] == 'o' || p[1] == 'O') &&
                   radix == 0 && (flags & ATOD_ACCEPT_BIN_OCT)) {
            p += 2;
            radix = 8;
        } else if ((p[1] == 'b' || p[1] == 'B') &&
                   radix == 0 && (flags & ATOD_ACCEPT_BIN_OCT)) {
            p += 2;
            radix = 2;
        } else if ((p[1] >= '0' && p[1] <= '9') &&
                   radix == 0 && (flags & ATOD_ACCEPT_LEGACY_OCTAL)) {
            int i;
            has_legacy_octal = TRUE;
            sep = 256;
            for (i = 1; (p[i] >= '0' && p[i] <= '7'); i++)
                continue;
            if (p[i] == '8' || p[i] == '9')
                goto no_prefix;
            p += 1;
            radix = 8;
        } else {
            goto no_prefix;
        }
        /* there must be a digit after the prefix */
        if (to_digit((uint8_t)*p) >= radix)
            goto fail;
    no_prefix: ;
    } else {
 no_radix_prefix:
        if (!(flags & ATOD_INT_ONLY) &&
            (atod_type == ATOD_TYPE_FLOAT64 ||
             atod_type == ATOD_TYPE_BIG_FLOAT) &&
            strstart(p, "Infinity", &p)) {
            if (atod_type == ATOD_TYPE_BIG_FLOAT) {
                bf_t *a;
                val = JS_NewBigFloat(ctx);
                if (JS_IsException(val))
                    goto done;
                a = JS_GetBigFloat(val);
                bf_set_inf(a, is_neg);
            } else {
                double d = 1.0 / 0.0;
                if (is_neg)
                    d = -d;
                val = JS_NewFloat64(ctx, d);
            }
            goto done;
        }
    }
    if (radix == 0)
        radix = 10;
    is_float = FALSE;
    p_start = p;
    while (to_digit((uint8_t)*p) < radix
           ||  (*p == sep && (radix != 10 ||
                              p != p_start + 1 || p[-1] != '0') &&
                to_digit((uint8_t)p[1]) < radix)) {
        p++;
    }
    if (!(flags & ATOD_INT_ONLY)) {
        if (*p == '.' && (p > p_start || to_digit((uint8_t)p[1]) < radix)) {
            is_float = TRUE;
            p++;
            if (*p == sep)
                goto fail;
            while (to_digit((uint8_t)*p) < radix ||
                   (*p == sep && to_digit((uint8_t)p[1]) < radix))
                p++;
        }
        if (p > p_start &&
            (((*p == 'e' || *p == 'E') && radix == 10) ||
             ((*p == 'p' || *p == 'P') && (radix == 2 || radix == 8 || radix == 16)))) {
            const char *p1 = p + 1;
            is_float = TRUE;
            if (*p1 == '+') {
                p1++;
            } else if (*p1 == '-') {
                p1++;
            }
            if (is_digit((uint8_t)*p1)) {
                p = p1 + 1;
                while (is_digit((uint8_t)*p) || (*p == sep && is_digit((uint8_t)p[1])))
                    p++;
            }
        }
    }
    if (p == p_start)
        goto fail;

    buf = buf1;
    buf_allocated = FALSE;
    len = p - p_start;
    if (unlikely((len + 2) > sizeof(buf1))) {
        buf = js_malloc_rt(ctx->rt, len + 2); /* no exception raised */
        if (!buf)
            goto mem_error;
        buf_allocated = TRUE;
    }
    /* remove the separators and the radix prefixes */
    j = 0;
    if (is_neg)
        buf[j++] = '-';
    for (i = 0; i < len; i++) {
        if (p_start[i] != '_')
            buf[j++] = p_start[i];
    }
    buf[j] = '\0';

    if (flags & ATOD_ACCEPT_SUFFIX) {
        if (*p == 'n') {
            p++;
            atod_type = ATOD_TYPE_BIG_INT;
        } else if (*p == 'l') {
            p++;
            atod_type = ATOD_TYPE_BIG_FLOAT;
        } else if (*p == 'm') {
            p++;
            atod_type = ATOD_TYPE_BIG_DECIMAL;
        } else {
            if (flags & ATOD_MODE_BIGINT) {
                if (!is_float)
                    atod_type = ATOD_TYPE_BIG_INT;
                if (has_legacy_octal)
                    goto fail;
            } else {
                if (is_float && radix != 10)
                    goto fail;
            }
        }
    } else {
        if (atod_type == ATOD_TYPE_FLOAT64) {
            if (flags & ATOD_MODE_BIGINT) {
                if (!is_float)
                    atod_type = ATOD_TYPE_BIG_INT;
                if (has_legacy_octal)
                    goto fail;
            } else {
                if (is_float && radix != 10)
                    goto fail;
            }
        }
    }

    switch(atod_type) {
    case ATOD_TYPE_FLOAT64:
        {
            double d;
            d = js_strtod(buf, radix, is_float);
            /* return int or float64 */
            val = JS_NewFloat64(ctx, d);
        }
        break;
    case ATOD_TYPE_BIG_INT:
        if (has_legacy_octal || is_float)
            goto fail;
        val = ctx->rt->bigint_ops.from_string(ctx, buf, radix, flags, NULL);
        break;
    case ATOD_TYPE_BIG_FLOAT:
        if (has_legacy_octal)
            goto fail;
        val = ctx->rt->bigfloat_ops.from_string(ctx, buf, radix, flags,
                                                pexponent);
        break;
    case ATOD_TYPE_BIG_DECIMAL:
        if (radix != 10)
            goto fail;
        val = ctx->rt->bigdecimal_ops.from_string(ctx, buf, radix, flags, NULL);
        break;
    default:
        abort();
    }

done:
    if (buf_allocated)
        js_free_rt(ctx->rt, buf);
    if (pp)
        *pp = p;
    return val;
 fail:
    val = JS_NAN;
    goto done;
 mem_error:
    val = JS_ThrowOutOfMemory(ctx);
    goto done;
}
