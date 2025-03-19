import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


def gauss_solve(A: list[list[float]], b: list[float]) -> list[float]:
    n = len(A)

    # k-й шаг
    for k in range(n - 1):
        # Строка с максимальным по модулю ведущим элементом в k-м столбце
        maxline = k
        for i in range(k + 1, n):
            if abs(A[i][k]) > abs(A[maxline][k]):
                maxline = i

        # Перестановка ak и amaxline
        if maxline != k:
            A[k], A[maxline] = A[maxline], A[k]
        # Перестановка bk и bmaxline
        b[k], b[maxline] = b[maxline], b[k]

        # Обнуление элементов ниже главного
        for i in range(k + 1, n):
            if A[k][k] == 0:
                raise ValueError("Нулевой ведущий элемент")
            m = A[i][k] / A[k][k]
            A[i][k] = 0.0
            for j in range(k + 1, n):
                A[i][j] -= m * A[k][j]
            b[i] -= m * b[k]

    # Обратный ход
    x = [0] * n
    for i in range(n - 1, -1, -1):
        s = sum(A[i][j] * x[j] for j in range(i + 1, n))
        if A[i][i] == 0:
            raise ValueError("На главной диагонали 0")
        x[i] = (b[i] - s) / A[i][i]

    return x


def spline_coeffs_find(
    x: list[float], y: list[float]
) -> tuple[list[int], list[int], list[float], list[int]]:
    n = len(x) - 1
    # Шаг (b - a) / 32:
    h: float = (x[n] - x[0]) / 32

    # A и b для трёхдиаг. системы относительно ci
    A = [[0] * (n + 1) for _ in range(n + 1)]
    rhs = [0] * (n + 1)

    # c0 = 0; c_n = 0
    A[0][0] = 1
    A[n][n] = 1
    rhs[0] = 0
    rhs[n] = 0

    # Строки трёхдиаг. системы
    # c{i-1} + 4ci + c{i+1} = (y{i+1} - 2yi + y{i-1}) / h^2
    for i in range(1, n):
        A[i][i - 1] = 1
        A[i][i] = 4
        A[i][i + 1] = 1

        rhs[i] = 3 * (y[i + 1] - 2 * y[i] + y[i - 1]) / (h**2)

    c_result = gauss_solve(A, rhs)

    a_vals = [0] * n
    b_vals = [0] * n
    d_vals = [0] * n

    for i in range(n):
        # ai = yi:
        a_vals[i] = y[i]

        # b_i = (y{i+1} - yi)/h - h/3 * (c{i+1} + 2ci)
        # d_i = (c{i+1} - ci) / (3 * h)
        if i != n - 1:
            b_vals[i] = (y[i + 1] - y[i]) / h - (
                c_result[i + 1] + 2 * c_result[i]
            ) * h / 3
            d_vals[i] = (c_result[i + 1] - c_result[i]) / (3 * h)
        else:
            b_vals[i] = ((y[n] - y[n - 1]) / h) - 2 * h * c_result[n - 1] / 3
            d_vals[i] = -1 * (c_result[n - 1]) / (3 * h)

    return a_vals, b_vals, c_result, d_vals


def cubic_spline_eval(
    x: list[float],
    a: list[float],
    b: list[float],
    c: list[float],
    d: list[float],
    X: float,
):
    # S_i(X) = a[i] + b[i]*(X - x[i])
    #               + c[i]*(X - x[i])^2
    #               + d[i]*(X - x[i])^3
    n = len(x) - 1

    if X < x[0] or X > x[n]:
        return None

    i = 0
    while i < n - 1 and not (x[i] <= X < x[i + 1]):
        i += 1

    dx = X - x[i]
    return a[i] + b[i] * dx + c[i] * (dx**2) + d[i] * (dx**3)


def plot(xs: list[float], ys: list[float]) -> None:
    plt.figure(figsize=(8, 5))
    plt.plot(
        xs,
        ys,
        marker="o",
        linestyle="-",
        color="b",
    )

    plt.xlabel("X")
    plt.ylabel("Y")
    plt.grid(True)

    plt.show()


def make_full_graph(
    a: list[float],
    b: list[float],
    c: list[float],
    d: list[float],
    left: float,
    right: float,
    x_nodes: list[float],
    step: float,
) -> None:
    i = left
    xs = []
    ys = []
    while i <= right:
        xs.append(i)
        ys.append(cubic_spline_eval(x_nodes, a, b, c, d, i))
        i += step
    plot(xs, ys)


def calc_vals(
    a: list[float],
    b: list[float],
    c: list[float],
    d: list[float],
    x_nodes: list[float],
) -> None:
    for i in range(1, len(x_nodes) + 1):
        point = (i - 1 / 2) * ((x_nodes[len(x_nodes) - 1] - x_nodes[0]) / 32)
        f_val = f(point)
        phi_val = cubic_spline_eval(x_nodes, a, b, c, d, point)
        print(
            f"f({point}) = {f_val}, "
            f"phi({point}) = {phi_val}, "
            f"|f({point}) - phi({point})| = {abs(phi_val - f_val)}"
        )


def f(x: float):
    return 8 * np.sin(0.5 * x) + 4 * np.cos(0.3 * x) - 0.2 * x


def main():
    x_nodes = list(range(32))
    y_nodes = [f(x) for x in x_nodes]

    # Таблица значений
    df = pd.DataFrame({"x": x_nodes, "y": y_nodes})
    print(df.to_string(index=False))

    a, b, c, d = spline_coeffs_find(x_nodes, y_nodes)

    print(
        "Массивы коэффициентов:\na: ",
        list(map(float, a)),
        "\nb: ",
        list(map(float, b)),
        "\nc: ",
        list(map(float, c)),
        "\nd: ",
        list(map(float, d)),
    )

    # График известных значений
    plot(x_nodes, y_nodes)

    # График сплайна
    make_full_graph(a, b, c, d, 0, 32, x_nodes, 0.1)

    print("Значения в точках x_i = a + (i + 1/2)h:")
    calc_vals(a, b, c, d, x_nodes)

    X = float(input("Введите точку (от 0 до 31): "))
    S_X = cubic_spline_eval(x_nodes, a, b, c, d, X)
    print(f"f({X}) = {f(X)}, сплайн в точке {X} равен {S_X}")


if __name__ == "__main__":
    main()
