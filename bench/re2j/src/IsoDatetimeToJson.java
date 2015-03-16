import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import com.google.re2j.Matcher;
import com.google.re2j.Pattern;

public class IsoDatetimeToJson {

    private static String regex = "((?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])T(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?";

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
                    System.out.print(String.format("{'year'='%s', 'month'='%s', 'day'='%s', 'hours'='%s', 'minutes'='%s', 'seconds'='%s', 'tz'='%s'}\n",
                                                   matcher.group(1), matcher.group(2), matcher.group(3), matcher.group(4), matcher.group(5), matcher.group(6), matcher.group(7)));
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
