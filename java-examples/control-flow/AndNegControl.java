public class AndNegControl {
  public static void main(String[] args) {
    try {
      int x = method(285212673, 45088768);
      System.out.println(x);
    } catch (Exception e) {
      System.out.println(e);
    }
  }

  static int method(int x, int y) {
    if (((-x) & (-y)) == 0) {
      throw new RuntimeException();
    } else {
      return 1;
    }
  }
}
