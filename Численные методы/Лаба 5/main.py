from sympy import *


# f(x) = 2 * x_1^2 + 3 * x_2^2 - 2 * sin((x_1 - x_2) / 2) + x_2
# x0 = (0, 0)


eps = 0.001


x_1 = Symbol("x_1")
x_2 = Symbol("x_2")
f = 2 * x_1**2 + 3 * x_2**2 - 2 * sin((x_1 - x_2) / 2) + x_2


def norm(point: dict):
    df_x1 = diff(f, x_1).subs(point).evalf()
    df_x2 = diff(f, x_2).subs(point).evalf()
    return max(abs(df_x1), abs(df_x2))


def phi_k_first(x_k):
    return (
        -(diff(f, x_1).subs(x_k) ** 2).evalf() - (diff(f, x_2).subs(x_k) ** 2).evalf()
    )


def phi_k_second(x_k):
    return (
        diff(f, x_1, 2).subs(x_k).evalf() * diff(f, x_1).subs(x_k).evalf() ** 2
        + 2
        * diff(f, x_1, x_2).subs(x_k).evalf()
        * diff(f, x_1).subs(x_k).evalf()
        * diff(f, x_2).subs(x_k).evalf()
        + diff(f, x_2, 2).subs(x_k).evalf() * diff(f, x_2).subs(x_k).evalf() ** 2
    )


def t_k(x_k):
    return -phi_k_first(x_k) / phi_k_second(x_k)


def grad_down(start_point: dict[Symbol, int]):
    x_k = x_k1 = start_point
    num_iter = 0
    while norm(x_k) >= eps:
        num_iter += 1
        x_k = x_k1
        x_k1 = {
            x_1: x_k[x_1] - t_k(x_k) * diff(f, x_1).subs(x_k).evalf(),
            x_2: x_k[x_2] - t_k(x_k) * diff(f, x_2).subs(x_k).evalf(),
        }
    return x_k1, num_iter


def find_min():
    df_dx1 = diff(f, x_1)
    df_dx2 = diff(f, x_2)
    critical_point = nsolve([df_dx1, df_dx2], [x_1, x_2], [0, 0])
    return critical_point


def main():
    start_point = {x_1: 5000, x_2: 5000}
    res_numeric, num_iter = grad_down(start_point)
    res_analytic = find_min()

    print(f"eps = {eps}")
    print(f"Число итераций: {num_iter}, начальная точка: ({start_point[x_1]}, {start_point[x_2]})")
    print(f"Точка минимума: (x, y) = ({res_numeric[x_1]:.6f}, {res_numeric[x_2]:.6f})")
    print(
        f"Точка минимума, вычисленная аналитически: (x, y) = ({res_analytic[0]:.6f}, {res_analytic[1]:.6f})"
    )
    print(
        f"Абсолютные погрешности. По x: {abs(res_numeric[x_1] - res_analytic[0]):.6f}; "
        f"по y: {abs(res_numeric[x_2] - res_analytic[1]):.6f}"
    )
    print(f"Норма в найденной точке минимума: {norm(res_numeric):.6f}")


if __name__ == "__main__":
    main()
