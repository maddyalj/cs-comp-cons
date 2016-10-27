/*
 * Testing debuging variables and expressions
 */

is_positive (x) {
    if (x > 0) {
        1;
    } else {
        dump(-100);
        0;
    }
}

main () {
    let x = -14;
    x * 2;
    dump(x);
    x = x + 4;
    dump(x);
    dump(x < 0);
    is_positive(x);
}
