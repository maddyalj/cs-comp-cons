/*
 * Testing complex constant folding with propagation
 */

main () {
    const n = 123;
    if (n < 100) {
        30;
    } else {
        n + 774;
    }
}
