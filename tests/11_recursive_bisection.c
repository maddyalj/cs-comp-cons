bisection (f, l, h, i) {
    let m = (l + h) / 2;
    if (i == 0) {
        return m;
    }

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
    i = i - 1;
    return bisection(f, l, h, i);
}
