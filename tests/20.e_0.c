/*
 * Testing evaluation of complex conditions
 */
main () {
    let a = 44;
    let b = a + 3;
    a > 1 && (a > b || b > a + 5);
}
