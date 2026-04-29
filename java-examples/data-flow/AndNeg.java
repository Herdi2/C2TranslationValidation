public class AndNeg{
    public static void main(String[] args) {
      AndNeg andneg = new AndNeg();
      System.out.println(andneg.method(-2, -2));
    }

    int method(int x, int y){
        return (-x) & (-y);
    }
}
