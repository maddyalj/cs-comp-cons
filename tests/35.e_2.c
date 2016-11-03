/*
 * Testing constant folding and propagation
 */

main () {
    const DEV_ENV = 0;
    const PROD_ENV = 1;
    const ENV = DEV_ENV;
    if (ENV == PROD_ENV) {
        5;
    } else {
        2;
    }
}
