package edu.diku.kmc.benchmarks.drex;

import java.util.Scanner;
import java.io.IOException;
import edu.upenn.cis.drex.bench.*;
import edu.upenn.cis.drex.core.expr.Expression;

// DReX version of "drex_swap-bibtex"

public class DReXSwapBibtex {

    public static void main(String [] args) {
        int lno = 0;
        long preCompile = System.currentTimeMillis();

        Expression expression = BenchExprs.buildBibtexSwapper();
        Evaluator evaluator = BenchmarkEvaluator.getForkjoinEvaluator("drex_swap-bibtex", expression);

        Scanner scanner = new Scanner(System.in).useDelimiter("\\Z");
        String input;

        long start = System.currentTimeMillis();
        try {
            input = scanner.next(); // read entire file into input
            String result = evaluator.eval(input);
            System.out.print(result);
        } catch(Exception e) {
            System.err.print(String.format("Exception received\n"));
        }
        long end = System.currentTimeMillis();

        long elaps = end - start;
        long elapsCompile = start - preCompile;

        System.err.print(String.format("\ncompilation (ms): %d\n", elapsCompile));
        System.err.print(String.format("matching (ms):    %d\n", elaps));
    }
}

