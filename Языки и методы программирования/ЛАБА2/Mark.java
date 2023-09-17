import java.util.Objects;

public class Mark {
    private int kolvo_stud;
    public int[] marks;

    public String[] names;

    Mark(int kolvo_stud){
        this.kolvo_stud = kolvo_stud;
    }

    public int get_kolvo(){
        return kolvo_stud;
    }
    public void create_group(){
        this.marks = new int[this.kolvo_stud];
        this.names = new String[this.kolvo_stud];
    }
    public void set_mark(int number, int ball, String name){
            marks[number]=ball;
            names[number]=name;
    }

    private int get_number(String str){
        for(int i=0; i<kolvo_stud; i++){
            if(Objects.equals(names[i], str)){
                return i+1;
            }
        }
        System.err.println("Такого студента не существует.\nДля выхода введите \"Выход\"");
        return -1;
    }
    private int get_mark(String str){
        int number = get_number(str);
        if(number==-1){
            return -1;
        }
        int mark=0;
        int ball = marks[number-1];
        double four = 4.0;
        double shag = this.kolvo_stud/four;
        int kolvo=0;
        for(int i=0; i<this.kolvo_stud; i++){
            if(marks[i]>ball && i!=(number-1)){
                kolvo+=1;
            }
        }

        if(kolvo<shag){
            mark = 5;
        } else if(kolvo<shag*2){
            mark = 4;
        } else if(kolvo<shag*3){
            mark = 3;
        } else{
            mark = 2;
        }
        return mark;
    }

    public String toString(String str){
        int mark = get_mark(str);
        if(mark == -1){
            return "";
        }
        return "("+mark+")";
    }
}
