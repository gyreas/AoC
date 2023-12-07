import java.io.*;
import java.nio.file.*;
import java.util.stream.*;

public class Trebucket1 {
   public static void main(String[] args) {
      var p = args[0];
      int sum = getSum(Paths.get(p));
      System.out.println("Sum = " + sum);
   }

   public static int twoDigit(String s) {
      var d1 = "";
      var d2 = "";

      for (char ch : s.toCharArray()) {
         if (Character.isDigit(ch)) {
            if (d1.isEmpty()) d1 = Character.toString(ch);
            else              d2 = Character.toString(ch);
         }
         // System.out.printf("%s , ", ch);
      }
      if (d2.isEmpty()) d2 = d1;

      var digits = new StringBuilder();

      digits.append(d1);
      digits.append(d2);

      return Integer.parseUnsignedInt(digits.toString());
   }

   public static int getSum(Path p){
      Stream<String> calibs = null;
      try {
         calibs = Files.lines(p);
      } catch (IOException x) {
         x.printStackTrace();
      }
      return calibs
               .mapToInt(line -> twoDigit(line.strip()))
               .sum();
   }
}
