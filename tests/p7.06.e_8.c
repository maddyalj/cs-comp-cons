/*
 * Testing const and op
 */
sum () {
    let y = 5;
    y;
}

main () {
    let x = 3;
    x = x + sum();
    x;
}
