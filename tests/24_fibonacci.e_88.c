/*
 * Testing evaluation of the sum of n fibonacci numbers
 */
main () {
    const n = 9; /* number of fibonacci numbers to calculate sum of */
    if (n == 0) {
        0;
    } else {
        let first = 0;
        let second = 1;
        let sum = 1;
        let i = 1;
        while (i < n) {
            let temp = second;
            second = first + second;
            first = temp;
            sum = sum + second;
            i = i + 1;
        }
        sum;
    }
}
