import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import com.google.re2j.Matcher;
import com.google.re2j.Pattern;

// RE2/J  version of "csv_project3"

public class CSVProject3 {

    private static String regex = "([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*),([^,\n]*)";

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
                    System.out.print(String.format("%s\t%s\n", matcher.group(2), matcher.group(5)));
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
