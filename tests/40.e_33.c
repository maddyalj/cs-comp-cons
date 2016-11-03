/*
 * Testing function inlining
 */

sum (x, y) {
    x + y;
}

main () {
    let z = sum(-3, 36);
    z;
}
