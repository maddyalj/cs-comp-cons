/*
 * Testing recursive function and extra comments
 */
factorial (n) {
    if (n == 0) {
        1; /* extra comment! */
    } else {
        n * factorial(n - 1);
    }
}
