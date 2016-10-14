/*
 * Testing with recursive implementation of root bisection algorithm
 */
bisection (f, l, h, i) {
    let found = 0;
    let result = 0;
    let m = (l + h) / 2;
    if (i == 0) {
        found = 1;
        result = m;
    }

    let fL = f(l);
    if (fL == 0) {
        found = 1;
        result = l;
    }

    let fH = f(h);
    if (fH == 0) {
        found = 1;
        result = h;
    }

    let fM = f(m);
    if (fM == 0) {
        found = 1;
        result = m;
    }

    if (fM * fL < 0) {
        h = m;
    } else {
        l = m;
    }
    i = i - 1;

    if (found == 1) {
        result;
    } else {
        bisection(f, l, h, i);
    }
}
