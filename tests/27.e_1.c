/*
 * Testing boolean-like returning functions
 */

is_positive (x) {
    if (x > 0) {
        1;
    } else {
        0;
    }
}

main () {
    is_positive(134);
}
