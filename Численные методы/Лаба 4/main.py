from sympy import *
import numpy as np
from matplotlib import pyplot as plt

# x^3 - 5x - 1 = 0
roots_real = [-2.12842, -0.20164, 2.33006]


eps = 0.001


x = Symbol('x')
f = x ** 3 - 5 * x - 1
count_steps_half = 0
count_steps_newton = 0


def half_method(a: float, b: float):
    global count_steps_half
    count_steps_half += 1

    # Тут модификация на редкий случай, если попали в корень точно
    if b - a < 2 * eps or f.subs(x, (a + b) / 2) == 0:
        return (a + b) / 2

    if f.subs(x, a) * f.subs(x, (a + b) / 2) < 0:
        return half_method(a, (a + b) / 2)
    if f.subs(x, (a + b) / 2) * f.subs(x, b) < 0:
        return half_method((a + b) / 2, b)


def newton_method(a: float, b: float):
    global count_steps_newton
    x_k = 0
    x_k_1 = 0
    if f.subs(x, a) * f.diff().diff().subs(x, a) > 0:
        x_k_1 = a
    elif f.subs(x, b) * f.diff().diff().subs(x, b) > 0:
        x_k_1 = b

    x_k = x_k_1 - f.subs(x, x_k_1) / f.diff().subs(x, x_k_1)
    count_steps_newton = 1

    while f.subs(x, x_k) * f.subs(x, x_k + sign(x_k - x_k_1) * eps) >= 0:
        count_steps_newton += 1
        x_k_1 = x_k
        x_k = x_k_1 - f.subs(x, x_k_1) / f.diff().subs(x, x_k_1)

    return x_k.evalf()


# По теореме Штурма (была на алгебре)
def find_segments():
    # Многочлен и система Штурма
    poly = Poly(f, x)
    seq = sturm(poly.as_expr(), x)

    # Оценка Коши (граница, где лежат все корни)
    coeffs = poly.all_coeffs()
    a_n = coeffs[0]
    others = coeffs[1:]
    bound = 1 + max(abs(c) / abs(a_n) for c in others)

    # Подсчёт числа смен знака в системе
    def var_count(val):
        vals = [p.subs(x, val).evalf() for p in seq]
        # нули пропускаются (по теореме)
        signs = [1 if v > 0 else -1 for v in vals if v != 0]
        return sum(1 for i in range(len(signs) - 1) if signs[i] != signs[i + 1])

    # Первые две производные
    f1 = f.diff(x)
    f2 = f.diff(x, 2)

    # Рекурсивная изоляция корней на отрезке
    def isolate(a, b):
        va, vb = var_count(a), var_count(b)
        roots_inside = va - vb
        if roots_inside == 0:
            return []

        m = (a + b) / 2
        if roots_inside > 1:
            return isolate(a, m) + isolate(m, b)

        # проверка, что f' и f'' не обнуляются на [a, b]
        d1a, d1b = f1.subs(x, a).evalf(), f1.subs(x, b).evalf()
        d2a, d2b = f2.subs(x, a).evalf(), f2.subs(x, b).evalf()
        cond1 = (d1a != 0 and d1b != 0 and d1a * d1b > 0)
        cond2 = (d2a != 0 and d2b != 0 and d2a * d2b > 0)
        if cond1 and cond2:
            return [(float(a), float(b))]
        else:
            # если хотя бы одна из производных обнуляется или меняет знак -- дробим дальше
            return isolate(a, m) + isolate(m, b)

    # Запуск на отрезке [-bound, +bound]
    return isolate(-bound, bound)


def plot_fun():
    f_num = lambdify(x, f, modules=["numpy"])

    x_vals = np.linspace(-3, 3, 1000)
    y_vals = f_num(x_vals)

    plt.plot(x_vals, y_vals, label='f(x) = x^3 - 5x - 1')
    plt.axhline(0, color='gray', linestyle='--')  # ось X
    plt.axvline(0, color='gray', linestyle='--')  # ось Y
    plt.title("График функции f(x)")
    plt.xlabel("x")
    plt.ylabel("f(x)")
    plt.grid(True)
    plt.legend()
    plt.show()


def main():
    plot_fun()

    segments = find_segments()
    global count_steps_half
    print(f'eps = {eps}')
    print(f'Отрезки, найденные по теореме Штурма: {segments}')

    roots_half = []
    roots_newton = []
    step_counts_half = []
    step_counts_newton = []
    for segment in segments:
        roots_half.append(half_method(segment[0], segment[1]))
        step_counts_half.append(count_steps_half)
        count_steps_half = 0

        roots_newton.append(newton_method(segment[0], segment[1]))
        step_counts_newton.append(count_steps_newton)

    for i in range(len(roots_half)):
        print(f'Корень номер {i + 1}. Реальное значение: {roots_real[i]}. '
              f'Метод деления пополам: {roots_half[i]:.5f}, число шагов: {step_counts_half[i]}. '
              f'Метод Ньютона: {roots_newton[i]:.5f}, число шагов: {step_counts_newton[i]}')
        print(f'Абсолютная погрешность. Для метода деления пополам: '
              f'{abs(roots_real[i] - roots_half[i]):.5f}, '
              f'для метода Ньютона: {abs(roots_real[i] - roots_newton[i]):.5f}')
        print()


if __name__ == '__main__':
    main()
