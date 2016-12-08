/*
 * Testing with iterative implementation of root bisection algorithm
 */

f(x) {0}

bisection (f, l, h, max_i) {
    let found = 0;
    let result = 0;
    let m = (l + h) / 2;
    let i = 0;
    while (i < max_i && found == 0) {
        let fL = f(l);
        if (fL == 0) {
            found = 1;
            result = l;
        } else {
            let fH = f(h);
            if (fH == 0) {
                found = 1;
                result = h;
            } else {
                let fM = f(m);
                if (fM == 0) {
                    found = 1;
                    result = m;
                } else {
                    if (fM * fL < 0) {
                        h = m;
                    } else {
                        l = m;
                    }

                    m = (l + h) / 2;
                    i = i + 1;
                }
            }
        }
    }
    result;
}

main () {
    0;
}
