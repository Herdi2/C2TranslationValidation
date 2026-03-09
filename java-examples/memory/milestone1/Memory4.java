class Memory4 {
  public static void main(String[] args) {
    (new Memory4()).method(10, 20);
  }

  int f1 = 10;
  int f2 = 20;
  int f3 = 30;

  int method(int x, int y) {
    int res1 = f1 + f2;
    int res2 = f2 + f3;
    if (x > y) {
      f1 = x;
    } else {
      f1 = y;
    }
    return res1 + res2 + f1;
  }

}
