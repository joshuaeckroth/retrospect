
package misc;

// from http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Java

public class LevenshteinDistance {
    private static int minimum(int a, int b, int c) {
        return Math.min(Math.min(a, b), c);
    }
 
    public static int ld(String[] seq1, String[] seq2) 
    {
        int[][] distance = new int[seq1.length+1][seq2.length+1];
 
        for (int i = 0; i <= seq1.length; i++)
            distance[i][0] = i;
        for (int j = 0; j <= seq2.length; j++)
            distance[0][j] = j;
 
        for (int i = 1; i <= seq1.length; i++)
            for (int j = 1; j <= seq2.length; j++)
            {
                int eq = seq1[i-1].equals(seq2[j-1]) ? 0 : 1; 
                distance[i][j] = minimum(distance[i-1][j] + 1,
                                         distance[i][j-1] + 1,
                                         distance[i-1][j-1] + eq);
            }
 
        return distance[seq1.length][seq2.length];
    }
}

