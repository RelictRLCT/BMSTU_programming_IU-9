import java.util.Objects;
import java.util.Random;
import java.util.Scanner;

public class Test {
    public static void main(String[] args){
        Random random = new Random();
        Scanner scanner = new Scanner(System.in);
        System.out.println("Введите размер учебной группы: ");
        int kolvo;
        kolvo = scanner.nextInt();
        Mark group22b = new Mark(kolvo);

        System.out.println("Введите имена студентов: ");

        String musor;
        musor = scanner.nextLine();

        group22b.create_group();
        for(int i=0; i<group22b.get_kolvo(); i++){
            String str;
            str = scanner.nextLine();
            group22b.set_mark(i, random.nextInt(0, 100), str);
        }

        System.out.println("Введите имя для вывода оценки: ");

        while(true){
            String str;
            str = scanner.nextLine();

            if (Objects.equals(str, "Выход")){
                break;
            }

            String out = group22b.toString(str);
            if(!Objects.equals(out, "")) {
                System.out.println(out);
            }
        }
    }
}
