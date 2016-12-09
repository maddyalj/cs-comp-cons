is_divisible_by(x, y) {
    if (x == 0) {
        1;
    } else {
        if (x < 0) {
            0;
        } else {
            is_divisible_by(x - y, y);
        }
    }
}

main () {
    let output = 0;
    let i = 1;
    while (i <= 100) {
        if (is_divisible_by(i, 15)) {
            output = 35;
            print output;
        } else {
            if (is_divisible_by(i, 3)) {
                output = 3;
                print output;
            } else {
                if (is_divisible_by(i, 5)) {
                    output = 5;
                    print output;
                } else {
                    output = i;
                    print output;
                }
            }
        }
        i = i + 1;
    }
    output;
}
