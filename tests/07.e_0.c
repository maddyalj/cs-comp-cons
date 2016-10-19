/*
 * Testing constants, complex prints and appl
 */
do_stuff (x, env) {
    const PRODUCATION = 5;
    if (env != PRODUCATION && x > 0) {
        x = 2;
        print (x + 4) / 5 + x;
    } else {
        print x;
    }
}

main () {
    let x = read_int();
    do_stuff(x, 1);
    0;
}
