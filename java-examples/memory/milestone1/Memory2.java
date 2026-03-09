class Memory2 {
  public static void main(String[] args) {
    (new Memory2()).method(true);
  }

  int f1 = 1;
  int f2 = 2;

  void method(boolean b) {
    if (b) {
      f1 = 10;
    } else {
      f2 = 20;
    }
  }
}
