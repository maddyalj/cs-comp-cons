/*
 * Testing function inlining with print statements and sequence folding
 */

mean (x, y) {
    (x + y) / 2;
}

main () {
    1;
    2;
    print mean(12, 545 + 245);
    3;
    4;
    mean(16, 18);
}
