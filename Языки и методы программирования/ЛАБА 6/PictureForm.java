import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class PictureForm {
    private JPanel mainPanel;
    private JSpinner highSpinner;
    private JTextField areaField;
    private CanvasPanel canvasPanel;
    private JSpinner diamSpinner;

    public PictureForm ( ) {

        highSpinner.setValue(5);
        diamSpinner.setValue(50);
        highSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent changeEvent) {
                int high = (int) highSpinner.getValue();
                canvasPanel.setHigh(high);
            }
        });
        diamSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent changeEvent) {
                int radius = (int) diamSpinner.getValue()/2;
                canvasPanel.setRadius(radius);
            }
        });
    }

    public static void main(String[] args) {
        JFrame frame = new JFrame("Бинарное дерево");
        frame.setContentPane(new PictureForm().mainPanel);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }
}
