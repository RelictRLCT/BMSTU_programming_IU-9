import math
# Интеграл = 8.0366775 - Вычислено калькулятором


def f(x: float) -> float:
    return math.log(2 * x)


eps = 0.001


def trap_method(a: float, b: float, n: int) -> float:
    h = (b - a) / n
    summ = 0
    for i in range(1, n, 1):
        summ += f(a + i * h)
    summ += (f(a) + f(b)) / 2
    return summ * h


def simpson_method(a: float, b: float, n: int) -> float:
    h = (b - a) / n
    sum1 = 0
    sum2 = 0
    for i in range(1, n, 2):
        sum1 += f(a + i * h)

    for i in range(2, n, 2):
        sum2 += f(a + i * h)

    summ = 4*sum1 + 2*sum2 + f(a) + f(b)
    return summ * h / 3


def average_rectangle(a: float, b: float, n: int) -> float:
    h = (b - a) / n
    summ = 0
    for i in range(0, n, 1):
        summ += f(a + i * h + h / 2)
    return summ * h


def runge_rule(S_h, S_2h, p):
    return (S_h - S_2h) / (2 ** p - 1)


def main():
    a = 0.5
    b = 2 * math.e

    n_t = 2
    S_2h = trap_method(a, b, n_t)
    S_h = trap_method(a, b, 2 * n_t)
    S_delta = runge_rule(S_h, S_2h, p=2)
    while abs(S_delta) > eps:
        n_t *= 2
        S_2h = trap_method(a, b, n_t)
        S_h = trap_method(a, b, 2 * n_t)
        S_delta = runge_rule(S_h, S_2h, p=2)

    n_s = 2
    S_2h_s = simpson_method(a, b, n_s)
    S_h_s = simpson_method(a, b, 2 * n_s)
    S_delta_s = runge_rule(S_h_s, S_2h_s, p=4)
    while abs(S_delta_s) > eps:
        n_s *= 2
        S_2h_s = simpson_method(a, b, n_s)
        S_h_s = simpson_method(a, b, 2 * n_s)
        S_delta_s = runge_rule(S_h_s, S_2h_s, p=4)

    n_s_t = 2
    S_2h_s_t = average_rectangle(a, b, n_s_t)
    S_h_s_t = average_rectangle(a, b, 2 * n_s_t)
    S_delta_s_t = runge_rule(S_h_s_t, S_2h_s_t, p=2)
    while abs(S_delta_s_t) > eps:
        n_s_t *= 2
        S_2h_s_t = average_rectangle(a, b, n_s_t)
        S_h_s_t = average_rectangle(a, b, 2 * n_s_t)
        S_delta_s_t = runge_rule(S_h_s_t, S_2h_s_t, p=2)

    print(f'     Метод трапеций    Метод Симпсона   Метод сред. прямоугольников')
    print(f'n          {n_t}                {n_s}                    {n_s_t}')
    print(f'I*         {S_h:.6f}          {S_h_s:.6f}          {S_h_s_t:.6f}')
    print(f'R          {S_delta:.6f}          {S_delta_s:.6f}         {S_delta_s_t:.6f}')
    print(f'I* + R     {S_h + S_delta:.6f}          {S_h_s + S_delta_s:.6f}          {S_h_s_t + S_delta_s_t:.6f}')


if __name__ == "__main__":
    main()
