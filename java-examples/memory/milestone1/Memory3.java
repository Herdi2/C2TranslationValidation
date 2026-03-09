class Memory3 {
  public static void main(String[] args) {
    (new Memory3()).method(true);
  }

  int f1 = 1;

  int method(boolean b) {
    if (b) {
      f1 = 10;
    } else {
      f1 = 20;
    }
    int x = f1 * 2;
    return x;
  }
}
