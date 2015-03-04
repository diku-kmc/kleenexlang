import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import com.google.re2j.Matcher;
import com.google.re2j.Pattern;

// RE2J (Java) version of the email validator

public class Email {

    // Same escaping for . as in the C++ version (two backslash)
    // No newline at the end, because readLine() does not return it.
    private static String regex = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?";

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
                    System.out.println(matcher.group(0));
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
