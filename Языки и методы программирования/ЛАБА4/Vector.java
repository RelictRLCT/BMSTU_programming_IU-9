import java.util.Iterator;
import java.util.Random;

public class Vector implements Iterable<Integer> {
    private int n;
    private int count;
    public int[][] vecs;
    Vector(int n, int count, int[][] vecs){
        this.n=n;
        this.count = count;
        this.vecs = vecs;
    }



    public Iterator iterator() {
        return new VectorIterator();
    }
    private class VectorIterator implements Iterator<Integer>{
        private int pos;
        public VectorIterator() {
            pos = 1 ;
        }
        public boolean hasNext() {
            return (pos < count);
        }

        public Integer next() {
            int mul = 0;
            for(int i = 0; i<n; i++){
                mul+=vecs[pos-1][i]*vecs[pos][i];
            }
            pos++;
            return mul;
        }
    }
}
