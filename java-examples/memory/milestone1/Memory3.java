class Memory3 {
  public static void main(String[] args) {
    method(true);
  }

  static int f1 = 1;

  static int method(boolean b) {
    if (b) {
      f1 = 10;
    } else {
      f1 = 20;
    }
    int x = f1 * 2;
    return x;
  }
}
