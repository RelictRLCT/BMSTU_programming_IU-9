from matplotlib import pyplot as plt
import numpy as np


def plot_fun(x_vals, y_vals, z, z_9):
    x_vals_z = np.linspace(1, 5, 1000)
    z_vals = z(x_vals_z)
    z_9_vals = z_9(x_vals_z)

    plt.plot(x_vals, y_vals, 'o', label='Точки таблично заданной функции')
    plt.plot(x_vals_z, z_vals, label='Функция, подобранная вручную')
    plt.plot(x_vals_z, z_9_vals, label='Функция, найденная программно')
    plt.axhline(0, color='gray', linestyle='--')  # ось X
    plt.axvline(0, color='gray', linestyle='--')  # ось Y
    plt.title("Точки из таблицы и z(x)")
    plt.xlabel("x")
    plt.ylabel("z(x)")
    plt.grid(True)
    plt.legend(loc='best')
    plt.show()


def calculate_vals(x_vals, y_vals, z):
    x_a = (x_vals[0] + x_vals[-1]) / 2
    y_a = (y_vals[0] + y_vals[-1]) / 2
    x_g = np.sqrt(x_vals[0] * x_vals[-1])
    y_g = np.sqrt(y_vals[0] * y_vals[-1])
    x_h = 2 / (1 / x_vals[0] + 1 / x_vals[-1])
    y_h = 2 / (1 / y_vals[0] + 1 / y_vals[-1])
    z_a = z(x_a)
    z_g = z(x_g)
    z_h = z(x_h)

    print(f'x_a = {x_a:.1f}, y_a = {y_a:.1f}, \n'
          f'x_g = {x_g:.1f}, y_g = {y_g:.1f}, \n'
          f'x_h = {x_h:.1f}, y_h = {y_h:.1f}, \n'
          f'z_a = {z_a:.1f}, z_g = {z_g:.1f}, z_h = {z_h:.1f} \n')

    delta_1 = abs(z_a - y_a)
    delta_2 = abs(z_g - y_g)
    delta_3 = abs(z_a - y_g)
    delta_4 = abs(z_g - y_a)
    delta_5 = abs(z_h - y_a)
    delta_6 = abs(z_a - y_h)
    delta_7 = abs(z_h - y_h)
    delta_8 = abs(z_h - y_g)
    delta_9 = abs(z_g - y_h)

    print(f'delta 1 ... 9: \n'
          f'{delta_1:.1f}, {delta_2:.1f}, {delta_3:.1f}, \n'
          f'{delta_4:.1f}, {delta_5:.1f}, {delta_6:.1f}, \n'
          f'{delta_7:.1f}, {delta_8:.1f}, {delta_9:.1f}')
    print(f'delta min: {min([delta_1, delta_2, delta_3,
                            delta_4, delta_5, delta_6,
                            delta_7, delta_8, delta_9]):.1f} \n')
    # delta_9 -- минимальное


def min_quad(x_vals, y_vals):
    u = np.log(x_vals)          # ln x_i
    w = 1 / np.array(y_vals)    # 1 / y_i

    n  = len(x_vals)
    S1 = np.sum(u * u)          # sum u_i^2
    S2 = np.sum(u)              # sum u_i
    S3 = np.sum(u * w)          # sum u_i * w_i
    S4 = np.sum(w)              # sum w_i

    det = S1 * n - S2 ** 2
    if abs(det) < 1e-12:
        raise ValueError("Система вырождается. Где-то ошибка")

    a = (S3 * n - S2 * S4) / det
    b = (S1 * S4 - S2 * S3) / det
    return a, b


def sko(x_vals, y_vals, z):
    n = len(x_vals)
    summ = 0
    for i in range(n):
        summ += (z(x_vals[i]) - y_vals[i]) ** 2
    return np.sqrt(summ) / np.sqrt(n - 1)


def main():
    x_vals = np.linspace(1, 5, 9)
    y_vals = [1.24, 1.74, 1.61, 2.16, 3.06, 2.88, 4.53, 5.40, 7.07]

    z = lambda x: 0.7 * np.exp(x * 0.44)

    print('z(x) = 0.7e^(0.44x)\n')

    calculate_vals(x_vals, y_vals, z) # delta_9 оказалось минимальным

    # z_9 = 1/(alnx + b)
    # 1 / z_9 = alnx + b

    a, b = min_quad(x_vals, y_vals)
    z_9 = lambda x: 1 / (a * np.log(x) + b)

    print(f'z_9(x) = 1 / ({a:.3f} ln(x) + {b:.3f})\n')

    plot_fun(x_vals, y_vals, z, z_9)

    print(f'СКО: {sko(x_vals, y_vals, z_9):.3}')


if __name__ == '__main__':
    main()
    
