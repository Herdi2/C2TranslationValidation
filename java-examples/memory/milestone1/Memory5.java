class Memory5 {
  public static void main(String[] args) {
    (new Memory5()).method(1, 2);
  }

  int f1 = 10;

  int method(int x, int y) {
    int a = f1;
    if (x > 10) {
      f1 = 12;
    } else {
      f1 = 24;
    }
    int b = f1;
    if (y < -5) {
      f1 = 36;
    } else {
      f1 = 48;
    }
    int c = f1;
    return a + b + c;
  }

}
