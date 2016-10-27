/*
 * Testing with recursive implementation of root bisection algorithm
 */
bisection (f, l, h, i) {
    let m = (l + h) / 2;
    if (i == 0) {
        m;
    } else {
        let fL = f(l);
        if (fL == 0) {
            l;
        } else {
            let fH = f(h);
            if (fH == 0) {
                h;
            } else {
                let fM = f(m);
                if (fM == 0) {
                    m;
                } else {
                    if (fM * fL < 0) {
                        h = m;
                    } else {
                        l = m;
                    }
                    i = i - 1;
                    bisection(f, l, h, i);
                }
            }
        }
    }
}

main () {
    0;
}
