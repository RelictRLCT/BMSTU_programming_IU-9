import java.util.Arrays;
import java.util.Scanner;

public class Test {
    public void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Введите количество точек: ");
        int count = scanner.nextInt();
        int[][] points = new int[count][2];
        for (int i = 0; i < count; i++) {
            System.out.println("Введите координаты точки номер " + (i + 1));
            points[i][0] = scanner.nextInt();
            points[i][1] = scanner.nextInt();
        }

        Circles[] a = new Circles[]{
                new Circles(0, 0, 1, points),
                new Circles(2, 2, 3, points),
                new Circles(1, 1, 2, points),
        };

        Arrays.sort(a);

        for(int i = 0; i<3; i++){
            System.out.println(a[i]);
        }
    }
}
