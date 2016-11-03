/*
 * Testing complex constant folding with propagation and sequence folding and let statements
 */

main () {
    const n = 123;
    if (n < 100) {
        44;
        let z = 56;
        z + 50;
        z = z + 54;
        n + 50;
    } else {
        345;
        3534;
        print 34;
        n - 99;
    }
}
