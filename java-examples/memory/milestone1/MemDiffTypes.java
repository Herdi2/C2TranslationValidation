class MemDiffTypes {
  public static void main(String[] args) {
    (new MemDiffTypes()).method(1, 2);
  }

  int f1;
  long f2;
  float f3;
  double f4;

  double method(int x, int y) {
    f1 = x;
    f2 = y;
    f3 = x;
    f4 = y;
    if (x > y) {
      return y;
    } else {
      return x;
    }
  }

}
