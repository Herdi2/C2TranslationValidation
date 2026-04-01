class BasicRead {
  public static void main(String[] args) {
    BasicRead br = new BasicRead();
    for (int i = 0; i < 20_000; i++) {
      br.method(i % 2 == 0, i % 2);
    }
  }

  int f1 = 10;

  int method(boolean flag, int x) {
    if (flag) {
      f1 = 12;
    } else {
      f1 = 11;
    }
    return f1 + x;
  }

}
