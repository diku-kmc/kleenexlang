import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

// Measure the speed of piping stdin to stdout without invoking RE2J at all!
public class RawSpeed {

    public static void main(String [] args) {
        
        BufferedReader bi = new BufferedReader(new InputStreamReader(System.in));
        String line;

        long start = System.currentTimeMillis();
        try {
            while((line = bi.readLine()) != null) {
                System.out.println(line);
            }
        } catch(IOException e) {
            System.err.print(String.format("IOException!\n"));
        }
        long end = System.currentTimeMillis();

        long elaps = end - start;

        System.err.print(String.format("\nmatching (ms):    %d\n", elaps));
    }
}
