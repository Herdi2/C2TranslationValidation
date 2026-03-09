class Object1 {
  public static void main(String[] args) {
    (new Object1()).method(10);
  }

  class B {
    int b1;
  }

  class A {
    B a1;
    int a2;

    public A(int b1) {
      this.a1 = new B();
      this.a1.b1 = b1;
      this.a2 = 10;
    }
  }

  A f1 = new A(1);
  A f2 = new A(2);

  int method(int x) {
    int res = 0;
    res += f1.a1.b1;
    f1 = f2;
    res += f1.a1.b1;
    return res;
  }
}
