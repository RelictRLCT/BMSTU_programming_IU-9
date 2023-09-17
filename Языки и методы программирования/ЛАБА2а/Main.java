import java.util.Scanner;

public class Main {
    public static void main(String[] args){

        Scanner scanner = new Scanner(System.in);
        System.out.println("Введите число объектов во Вселенной: ");
        int value = scanner.nextInt();
        University un = new University(value);

        un.main(null);

    }
}
