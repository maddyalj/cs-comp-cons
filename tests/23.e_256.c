/*
 * Testing evaluation of nested if statement and while loop
 */
main () {
    let n = 2;
    let i = 103;
    if (i > 100) {
        while (i > 96) {
            n = n * 2;
            i = i - 1;
        }
        n;
    } else {
        if (i < 70) { n } else { i / 3 + n }
    }
}
