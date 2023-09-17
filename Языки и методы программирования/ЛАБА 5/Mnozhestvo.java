import java.util.*;
import java.util.stream.*;
public class Mnozhestvo {
    private HashSet<Otr> otrs;

    Mnozhestvo() {
        otrs = new HashSet<>();
    }

    public void add(Otr otr) {
        otrs.add(otr);
    }

    public Optional<Otr> otrsOptional() {
        Optional<Otr> result = Optional.empty();
        result = otrs.stream().filter(x -> x.a==x.b).max(Comparator.comparing(Otr::right));
        return result;
    }

    public Stream<Otr> otrsStream() {
        ArrayList<Otr> result = new ArrayList<>();
        otrs.stream()
                .filter(x -> otrs.stream().anyMatch(y -> x.b >= y.b && x.a <= y.b && x!=y))
                .forEach(x -> result.add(x));

        return result.stream();
    }
}
