public class AndNeg {
  public static void main(String[] args) {
    int x = method(-3, -4);
    System.out.println(x);
  }

  static int method(int x, int y) {
    return (-x) & (-y);
  }
}
