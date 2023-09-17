import java.util.Random;

public class Test {
    public static void main(String[] args){
        Mnozhestvo m = new Mnozhestvo();
        m.add(new Otr(1, 2));
        m.add(new Otr(2, 2));
        m.add(new Otr(-2, 3));
        m.add(new Otr(-32, 6));
        m.add(new Otr(0, 0));
        m.add(new Otr(-12, -11));
        m.add(new Otr(20, 20));
        System.out.println(m.otrsOptional());
        System.out.println("");
        m.otrsStream().forEach(System.out::println);
    }
}
