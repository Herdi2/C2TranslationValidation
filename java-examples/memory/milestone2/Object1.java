class Object1 {
  public static void main(String[] args) {
    Object1 obj = new Object1();
    for (int i = 0; i < 20_000; i++) {
      obj.method(i % 2);
    }
  }

  class A {
    int a1 = 20;
    int a2 = 30;
  }

  A f1 = new A();

  public int method(int x) {
    if (x == 12) {
      return f1.a1;
    } else {
      return 10;
    }
  }
}
