import math
from copy import deepcopy

import numpy as np

# y'' + p y' + q y = f(x)
# Вариант 22: p = -1, q = 0, f(x) = 3, y0 = 0, y0' = 2
# y'' - y' = 3
#
# Система: u' = v
#          v' = v + 3
# u(0) = 0, v(0) = 2
#
# Точное решение: u(1) = 5.59141 , v(1) = 10.59141
u_1_true = 5.59141
v_1_true = 10.59141

eps = 0.001


def u(t):
    return 5 * math.exp(t) - 3 * t - 5


def v(t):
    return 5 * math.exp(t) - 3


def f(x, y):
    u, v = y
    f1 = v       # u' = v
    f2 = v + 3   # v' = v + 3
    return np.array([f1, f2])


def runge_kutta_step(f, x, y, h):
    k1 = f(x, y)
    k2 = f(x + h / 2, y + h * k1 / 2)
    k3 = f(x + h / 2, y + h * k2 / 2)
    k4 = f(x + h, y + h * k3)
    return y + (h / 6) * (k1 + 2 * (k2 + k3) + k4)


def norm_one(a, b):
    max_val = 0
    for i in range(len(a)):
        cur_val = abs(a[i] - b[i])
        if cur_val > max_val:
            max_val = cur_val
    return max_val


def norm_full(a, b):
    max_val = 0
    for i in range(len(a)):
        try:
            cur_val = norm_one(a[i], b[2 * i])
        except IndexError:
            cur_val = 0
        if cur_val > max_val:
            max_val = cur_val
    return max_val


def runge_rule(y_h, y_2h, p):
    return norm_full(y_h, y_2h) / (2 ** p - 1)


def get_new_step(h, err, p):
    try:
        return 0.9 * h * (eps / err) ** (1 / (p + 1))
    except ZeroDivisionError:
        return 0.9 * h


def runge_solve(f, y0, x0, xend, h):
    x = x0
    y = y0
    y_vals = []
    x_vals = []
    y_real = []
    while x < xend:
        #print(h, x)
        if x + h - xend >= 0:
            y_vals.append(y)
            x_vals.append(x)
            y_real.append([u(x), v(x)])
            h = xend - x  # чтобы не перепрыгнуть через xend
            x = x + h
            y = runge_kutta_step(f, x, y, h)
            y_vals.append(y)
            x_vals.append(x)
            y_real.append([u(x), v(x)])
            break
        y_vals.append(y)
        x_vals.append(x)
        y_real.append([u(x), v(x)])
        x = x + h
        y = runge_kutta_step(f, x, y, h)
    return np.array(y_vals), np.array(y_real), np.array(x_vals)


def runge_kutta_with_autostep(f, y0, x0, xend, h):
    # Порядок метода
    p = 4

    y_h, _, _ = runge_solve(f, y0, x0, xend, h)
    y_h_2, y_r, xs = runge_solve(f, y0, x0, xend, h / 2)
    err = runge_rule(y_h, y_h_2, p)

    while err > eps:

        h = get_new_step(h , err, p)
        # print(h, err)
        y_h, _, _ = runge_solve(f, y0, x0, xend, h)
        y_h_2, y_r, xs = runge_solve(f, y0, x0, xend, h / 2)
        err = runge_rule(y_h, y_h_2, p)

    return np.array(y_h_2), np.array(y_r), np.array(xs)


def main():
    x0 = 0
    xend = 1

    y0 = np.array([0.0, 2.0]) # Н.у. u(0)=0, v(0)=2
    h = (xend - x0) / 2.0

    y, y_real, x = runge_kutta_with_autostep(f, y0, x0, xend, h)

    for i in range(len(y)):
        print(f'x = {x[i]:.8f}, [u(x), v(x)] приближённо = [{y[i][0]:.8f}, {y[i][1]:.8f}], '
              f'точное значение [u(x), v(x)] = [{y_real[i][0]:.8f}, {y_real[i][1]:.8f}], '
              f'абсолютная погрешность: {norm_one(y_real[i], y[i]):.8f}')
    print()

    print(f'eps = {eps}')
    print(f"Точные значения: u(1) = {u(xend):.8f}, v(1) = {v(xend):.8f}")
    print(f"Вычислено методом Рунге-Кутта: u(1) = {y[-1][0]:.8f}, v(1) = {y[-1][1]:.8f}")


if __name__ == '__main__':
    main()
