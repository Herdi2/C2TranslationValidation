public class SideEffect {
  public static void main(String[] args) {
    int x = method(1, 4);
    System.out.println(x);
  }

  // before
  static int method(int x, int y) {
    if (((-x) & (-y)) == -4) {
      return 140;
    } else {
      return 120;
    }
  }
}
