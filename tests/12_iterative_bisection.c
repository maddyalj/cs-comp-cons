bisection (f, l, h, max_i) {
    let m = (l + h) / 2;
    let i = 0;
    while (i < max_i) {
        let fL = f(l);
        if (fL == 0) {
            return l;
        }

        let fH = f(h);
        if (fH == 0) {
            return h;
        }

        let fM = f(m);
        if (fM == 0) {
            return m;
        }

        if (fM * fL < 0) {
            h = m;
        } else {
            l = m;
        }

        m = (l + h) / 2;
        i = i + 1;
    }
    return m;
}
