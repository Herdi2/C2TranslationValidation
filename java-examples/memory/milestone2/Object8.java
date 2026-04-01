class Object8 {
  public static void main(String[] args) {
    Object8 obj = new Object8();
    for (int i = 0; i < 20_000; i++) {
      obj.method(i % 2);
    }
  }

  class A {
    int a1;
  }

  class B {
    int b1;
    A b2 = new A();
  }

  A f1 = new A();
  B f2 = new B();

  int method(int x) {
    return x + f1.a1 + f2.b1 + f2.b2.a1 + f1.a1;
  }
}
