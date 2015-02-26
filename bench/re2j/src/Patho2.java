import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import com.google.re2j.Matcher;
import com.google.re2j.Pattern;

// RE2/J  version of "patho2"

public class Patho2 {

    private static String regex = "^(?:([a-z]*a)|([a-z]*b))?(?:\n)?$";

    public static void main(String [] args) {
        int lno = 0;
        long preCompile = System.currentTimeMillis();
        Pattern pattern = Pattern.compile(regex, Pattern.DOTALL);
        Matcher matcher;
        
        BufferedReader bi = new BufferedReader(new InputStreamReader(System.in));
        String line;

        long start = System.currentTimeMillis();
        try {
            while((line = bi.readLine()) != null) {
                lno++;
                matcher = pattern.matcher(line);
                if(matcher.matches()) {
                    System.out.print(String.format("%s\n", matcher.group(2) != null ? matcher.group(2) : ""));
                } else {
                    System.err.print(String.format("match error on line %d\n", lno));
                }
            }
        } catch(IOException e) {
            System.err.print(String.format("IOException at line %d\n", lno));
        }
        long end = System.currentTimeMillis();

        long elaps = end - start;
        long elapsCompile = start - preCompile;

        System.err.print(String.format("\ncompilation (ms): %d\n", elapsCompile));
        System.err.print(String.format("matching (ms):    %d\n", elaps));
    }
}
