/*
 * Testing function inlining with constant propagation and folding
 */

mult (x, y) {
    x * y;
}

main () {
    const z = mult(-2, 36);
    z + 72 + 62;
}
