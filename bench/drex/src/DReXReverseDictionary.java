package edu.diku.kmc.benchmarks.drex;

import java.util.Scanner;
import java.io.IOException;
import edu.upenn.cis.drex.bench.*;
import edu.upenn.cis.drex.core.expr.Expression;

// DReX version of "drex_rev-dict"

public class DReXReverseDictionary {

    public static void main(String [] args) {
        int lno = 0;
        long preCompile = System.currentTimeMillis();

        Expression expression = BenchExprs.buildReverse();
        Evaluator evaluator = BenchmarkEvaluator.getForkjoinEvaluator("drex_rev-dict", expression);

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

