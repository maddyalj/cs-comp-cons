/*
 * Testing larger than opcode, if else statement, nested if statements
 * and empty if statement
 */
max (a, b) {
    if (a > b) {
        if (a > 1) { a + 1; } else { a; }
    } else {
        b;
    }
}
