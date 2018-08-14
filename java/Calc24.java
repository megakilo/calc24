import java.util.*;
import java.util.stream.*;

public class Calc24 {
    static class Number {
        public final Float val;
        public final String expr;
        Number(Float val, String expr) {
            this.val = val;
            this.expr = expr;
        }
    }

    static class Pair<T> {
        public final List<T> taken;
        public final List<T> nontaken;
        Pair(List<T> t, List<T> nt) {
            taken = t;
            nontaken = nt;
        }
    }

    public static void main(String[] args) {
        Random rand = new Random();
        IntStream.range(0, 1000).forEach(i -> {
            List<Integer> numbers = IntStream.range(0, 4).mapToObj(j -> rand.nextInt(13) + 1)
                                                         .collect(Collectors.toList());
            String result = calc(numbers.stream().map(x -> new Number((float) x, x.toString()))
                                                 .collect(Collectors.toList()))
                                .filter(x -> x.val == 24).findAny()
                                .map(x -> x.expr).orElse("No Solution");                                        
            System.out.println(numbers + " -> " + result);
        });
    }

    static private Stream<Number> calc(List<Number> nums) {
        if (nums.size() == 1) return nums.stream();
        return reduce(nums).flatMap(xs -> calc(xs));
    }

    static private Stream<List<Number>> reduce(List<Number> nums) {
        return split(nums, 2).flatMap(pair -> combine2(pair.taken).map(i -> 
            Stream.concat(pair.nontaken.stream(), Stream.of(i)).collect(Collectors.toList())));
    }
    
    static private Stream<Number> combine2(List<Number> nums) {
        Stream<Number> result = Stream.of(new Number(nums.get(0).val + nums.get(1).val, 
                String.format("(%s + %s)", nums.get(0).expr, nums.get(1).expr)), 
            new Number(nums.get(0).val * nums.get(1).val, 
                String.format("(%s * %s)", nums.get(0).expr, nums.get(1).expr)));
        if (nums.get(0).val > nums.get(1).val) {
            result = Stream.concat(result, Stream.of(new Number(nums.get(0).val - nums.get(1).val, 
                String.format("(%s - %s)", nums.get(0).expr, nums.get(1).expr))));
        } else {
            result = Stream.concat(result, Stream.of(new Number(nums.get(1).val - nums.get(0).val, 
                String.format("(%s - %s)", nums.get(1).expr, nums.get(0).expr))));
        }
        if (nums.get(1).val != 0) {
            result = Stream.concat(result, Stream.of(new Number(nums.get(0).val / nums.get(1).val, 
                String.format("(%s / %s)", nums.get(0).expr, nums.get(1).expr))));
        }
        if (nums.get(0).val != 0) {
            result = Stream.concat(result, Stream.of(new Number(nums.get(1).val / nums.get(0).val, 
                String.format("(%s / %s)", nums.get(1).expr, nums.get(0).expr))));
        }
        return result;
    }

    static private <T> Stream<Pair<T>> split(List<T> nums, int n) {
        if (n == 0) return Stream.of(new Pair<T>(Collections.emptyList(), nums));
        if (nums.size() <= n) return Stream.of(new Pair<T>(nums, Collections.emptyList()));
        T head = nums.get(0);
        List<T> tail = nums.subList(1, nums.size());
        return Stream.concat(split(tail, n).map(pair -> new Pair<T>(
                pair.taken, Stream.concat(pair.nontaken.stream(), Stream.of(head)).collect(Collectors.toList()))),
            split(tail, n-1).map(pair -> new Pair<T>(
                Stream.concat(pair.taken.stream(), Stream.of(head)).collect(Collectors.toList()), pair.nontaken)));
    }
}
