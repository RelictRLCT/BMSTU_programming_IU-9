import java.util.Random;

public class Test {
    public static void main(String[] args){
        int [][] vecs = new int[5][10];
        for(int i = 0; i<5; i++) {
            Random random = new Random();
            for (int j = 0; j < 10; j++) {
                vecs[i][j] = random.nextInt(100);
            }
        }
        Vector vector = new Vector(10, 5, vecs);
        for(int a : vector){
            System.out.println(a);
        }
    }
}
