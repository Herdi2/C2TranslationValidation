class Memory2 {
  public static void main(String[] args) {
    Memory2 mem = new Memory2();
    for (int i = 0; i < 20_000; i++) {
      mem.method(i % 2 == 0, i % 2);
    }
  }

  int f1 = 1;
  int f2 = 2;

  int method(boolean b, int x) {
    if (b) {
      f1 = 10;
    } else {
      f1 = 20;
    }
    return f1 + x;
  }
}
