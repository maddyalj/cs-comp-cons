/*
 * Testing evaluation of const and if statement with multiple conditions
 */
main () {
    const LIVE = 5;
    const ENVIRONMENT = 1;
    let x = 45;
    if (ENVIRONMENT == LIVE || x < 10) {
        x = x + 1;
    } else {
        let y = 66;
        x = y;
    }
    x;
}
