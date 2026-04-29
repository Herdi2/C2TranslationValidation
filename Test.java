// Generated with seed 14725905904395840929
class Test {
    public static void main(String[] args) {
        Test t = new Test();
        for (int i = -20_000; i < 20_000; i++) {
            t.method(i);
            t.method2(i);
        }
    }

  int f1 = 0;

  int method(int x) {
    if (x >= 0) {
      f1 = 1;
    } else if (x - x + x != x) {
      f1 = 2;
    } else {
      f1 = 3;
    }
    return x;
  }

  int method2(int x) {
    if (x > 0) {
      return 1;
    } else {
      return 2;
    }
  }
}
