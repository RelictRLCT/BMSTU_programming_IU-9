import javax.swing.*;
import java.awt.*;

import static java.lang.Math.pow;

//ВАРИАНТ 22
public class CanvasPanel extends JPanel {
    private int radius = 25;
    private int high = 5;
    public void setRadius(int r) {
        radius = r;
        repaint();
    }
    public void setHigh(int h) {
        high = h;
        repaint();
    }
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        g.setColor(Color.RED);
        for(int i=0; i<high; i++){
            int kolvo = (int)pow(2, i);
            int dist = 1000/(kolvo+1);
            for(int j=0; j<pow(2, i); j++){
                g.drawOval(dist,i*100,radius,radius);
                g.fillOval(dist,i*100,radius,radius);
                if(i!=high-1){
                    g.drawLine(dist+radius/2, i*100, 2*(1000/((int)pow(2, i+1)+1))*(j+1)-(1000/((int)pow(2, i+1)+1)), (i+1)*100+radius/2);
                    g.drawLine(dist+radius/2, i*100, 2*(1000/((int)pow(2, i+1)+1))*(j+1), (i+1)*100+radius/2);
                }
                dist+=1000/(kolvo+1);
            }
        }
    }
}
