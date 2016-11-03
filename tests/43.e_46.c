/*
 * Testing function inlining with let and const
 */

increment (x) {
    x + 1;
}

main () {
    let z = 45;
    increment(z);
    const Y = 45;
    increment(Y);
}
