public class SideEffect {
  public static void main(String[] args) {
    try {
      int x = sideeffect(2, 2);
      System.out.println(x);
    } catch (Exception e) {
      System.out.println("exception");
    }
  }

  static int sideeffect(int x, int y) {
    if (((-x) & (-y)) == -2) {
      return 140;
    } else {
      return 120;
    }
  }
}
