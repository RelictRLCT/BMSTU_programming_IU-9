public class Otr {
    double a;
    double b;
    Otr(double a, double b){
        this.a = a;
        this.b = b;
    }

    public double right(){
        return b;
    }

    public String toString() {
        return "[" + a + ", " + b + "]";
    }
}
