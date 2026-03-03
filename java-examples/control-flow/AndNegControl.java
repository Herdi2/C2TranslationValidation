public class AndNegControl {
  public static void main(String[] args) {
    int x = method(285212673, 45088768);
    System.out.println(x);
  }

  static int method(int x, int y) {
    if (((-x) & (-y)) == 0) {
      throw new RuntimeException();
    } else {
      return 1;
    }
  }
}
