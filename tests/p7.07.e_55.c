/*
 * Testing let, while, asg, op and seq
 */

main () {
    let i = 10;
    let sum = 0;
    while (i) {
        sum = sum + i;
        i = i - 1;
    }
    sum;
}
