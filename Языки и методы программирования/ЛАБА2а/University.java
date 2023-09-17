import java.util.Scanner;

public class University {
    private static int kolvo;
    private Point[] points;
    public class Point{
        private int[] koords = new int[3];
        public Point(int k1, int k2, int k3){
                this.koords[0]=k1;
                this.koords[1]=k2;
                this.koords[2]=k3;
        }
    }
    public University(int kolvo){
        University.kolvo = kolvo;
    }

    private double[] get_napr(){
        double[] koords = new double[3];
        for(int i = 0; i<kolvo; i++) {
            koords[0]+=points[i].koords[0];
            koords[1]+=points[i].koords[1];
            koords[2]+=points[i].koords[2];
        }
        for(int i=0; i<3; i++){
            koords[i]/=kolvo;
        }
        return koords;
    }

    public void main(String[] args){
        points = new Point[kolvo];
        Scanner scanner = new Scanner(System.in);

        for(int i = 0; i<kolvo; i++){
            System.out.println("Введите три координаты: ");
            int k1 = scanner.nextInt();
            int k2 = scanner.nextInt();
            int k3 = scanner.nextInt();
            points[i] = new Point(k1, k2, k3);
        }

        System.out.println("Координаты среднего вектора: "+ get_napr()[0] + " "+ get_napr()[1]+ " " + get_napr()[2]);
    }
}
