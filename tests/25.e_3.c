/*
 * Testing let, and functions with 1 parameters
 */

increment (num) {
    num + 1;
}

main () {
    let x = 2;
    x = increment(x);
    x;
}
