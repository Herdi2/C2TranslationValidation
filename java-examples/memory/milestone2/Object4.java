class Object4 {

  public static void main(String[] args) {
    Object4 obj = new Object4();
    for (int i = 0; i < 20_000; i++) {
      obj.method(i % 2, i % 2, i % 2 == 0 ? null : obj.new A());
    }
  }

  class A {
    int x;
  }

  A f1 = new A();
  A f2 = new A();

  public int method(int x, int y, A a) {
    if (a != null) {
      return f1.x + x;
    } else {
      return f2.x + y;
    }
  }
}
