import static java.lang.Math.pow;
import static java.lang.Math.sqrt;

public class Circles implements Comparable<Circles> {
    public int[] coords = new int[2];
    private int radius;
    private int[][] points;

    public Circles(int c1, int c2, int radius, int[][] points){
        this.coords[0]=c1;
        this.coords[1]=c2;
        this.radius=radius;
        this.points=points;
    }

    private int get_kolvo_points(){
        int kolvo = 0;
        for(int i=0; i<points.length; i++){
            if(sqrt((points[i][0]-coords[0])*(points[i][0]-coords[0])
            + (points[i][1]-coords[1])*(points[i][1]-coords[1])) <= radius){
                kolvo+=1;
            }
        }
        return kolvo;
    }
    public int compareTo(Circles obj) {
        if (obj.get_kolvo_points() == this.get_kolvo_points()) return 0;
        else if (this.get_kolvo_points() < obj.get_kolvo_points()) return -1;
        else if (this.get_kolvo_points() > obj.get_kolvo_points()) return 1;
        return 20;
    }

    public String toString(){
        return coords[0] + " " + coords[1];
    }
}
