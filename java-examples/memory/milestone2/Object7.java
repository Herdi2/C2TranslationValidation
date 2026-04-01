class Object7 {
  public static void main(String[] args) {
    Object7 obj = new Object7();
    for (int i = 0; i < 10_000; i++) {
      obj.method(i % 2 == 0, i % 3 == 0, i % 4 == 0, obj.new A());
    }
  }

  class A {
    int x;
  }

  A f1 = new A();
  A f2 = new A();

  int method(boolean flag1, boolean flag2, boolean flag3, A a) {
    if (flag1) {
      f1 = a;
    }
    if (flag2) {
      f1 = f2;
    }
    return f1.x;
  }

}
