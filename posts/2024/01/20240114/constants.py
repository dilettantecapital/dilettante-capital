import math
from decimal import Decimal


# FUNCTIONS


def recurse_phi(n):
    phi = Decimal(1)
    for i in range(int(n)):
        phi = 1 + 1 / phi
    return phi


def recurse_e(n):
    e = Decimal(1)
    for i in range(int(n)):
        e += Decimal(1) / math.factorial(1 + i)
    return e


def newtons_root(x, y = None, limit = 995):
    if y is None:
        y = Decimal(x)
    if x == (y * y) or limit <= 0:
        return y
    else:
        return newtons_root(x, (y + x / y) / 2, limit - 1)


def test_precision(f, start = 1, limit = 1e6):
    hold = 0
    for n in range(int(start), int(limit)):
        test = f(n)
        if test == hold:
            break
        hold = test
    print(n, test)


# SCRIPT


if __name__ == "__main__":
    test_precision(recurse_phi)
    test_precision(recurse_e)

    x = newtons_root(47)
    print("  ", x * x)

    print("--")

    phi = recurse_phi(67)
    e = recurse_e(27)

    print(1 + phi)
    print(phi * phi)
    print(e)
