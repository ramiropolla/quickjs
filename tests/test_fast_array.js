"use strict";

function assert(actual, expected, message) {
    if (arguments.length == 1)
        expected = true;

    if (actual === expected)
        return;

    if (actual !== null && expected !== null
    &&  typeof actual == 'object' && typeof expected == 'object'
    &&  actual.toString() === expected.toString())
        return;

    throw Error("assertion failed: got |" + actual + "|" +
                ", expected |" + expected + "|" +
                (message ? " (" + message + ")" : ""));
}

function print_empty_items(i, n)
{
    let str = ", <";
    if ( n <= 0 )
        str = " <";
    const n_missing = i - n;
    str += JSON.stringify(n_missing);
    str += " empty item";
    if ( n_missing > 1 )
        str += "s";
    str += ">";
    return str;
}

function print_item(i, v)
{
    let str = ", ";
    if ( i === 0 )
        str = " ";
    str += JSON.stringify(v);
    return str;
}

function print_array(a)
{
    if ( is_node )
    {
        console.log(a);
        return;
    }

    const a_length = a.length;
    if ( a_length === 0 )
    {
        console.log("[]");
        return;
    }

    // console.log("length: " + a_length);
    let str = "[";
    let n = -1;
    a.forEach((v, i) => {
        // console.log(i, v);
        const next = n + 1;
        if ( i !== next )
            str += print_empty_items(i, next);
        str += print_item(i, v);
        n = i;
    });
    // console.log(n);
    const last = n + 1;
    if ( a_length !== last )
        str += print_empty_items(a_length - 1, n);
    str += " ]";
    console.log(str);
}

function pretty_print(element)
{
    console.log(JSON.stringify(element));
}

function test_array()
{
    print_array(new Array());
    print_array(new Array(3, 4, 5));
    print_array(new Array(10));

    let a = new Array(10); a[1] = 10; a[7] = 11; a;
    print_array(a);

    // [ <1 empty item>, 10, <5 empty items>, 11, <2 empty items> ]

    return;

    pretty_print(FastArray());
    pretty_print(FastArray(3));
    pretty_print(FastArray(3, 4, 5, 6, 7, 8));

    let fa = new FastArray();
    pretty_print(fa);
    fa.push(5, 6, 8);
    fa.unshift(-1, -2);
    pretty_print(fa);
    fa[1] = 7;
    pretty_print(fa);
    fa[9] = 5;
    pretty_print(fa);
}

let is_node = false;
try {
    if ( process.argv )
        is_node = true;
} catch {
}
test_array();
