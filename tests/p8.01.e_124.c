/*
 * Testing let, while, asg, op and seq
 */

x (n) {
    if (n >= 10 && n <= 10 + 7 - 1) {
        0;
    } else {
        n + 5;
    }
}

h (n) {
    if (n >= 7 && n <= 10 + 7 - 1) {
        0;
    } else {
        n * 2 - 3;
    }
}

main () {
    const SAMPLE_SIZE = 10;
    const KERNEL_SIZE = 7;
    let y_sum = 0;
    let n = 0;
    while (n < SAMPLE_SIZE + KERNEL_SIZE - 1) {
        let k = 0;
        while (k <= n) {
            y_sum = y_sum + x(k) * h(n - k);
            k = k + 1;
        }
        n = n + 1;
    }
    y_sum / (SAMPLE_SIZE + KERNEL_SIZE - 1);
}
