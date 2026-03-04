class Memory2 {
  public static void main(String[] args) {
    method(true);
  }

  static int f1 = 1;
  static int f2 = 2;

  static void method(boolean b) {
    if (b) {
      f1 = 10;
    } else {
      f2 = 20;
    }
  }
}
