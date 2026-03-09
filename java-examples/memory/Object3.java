class Object3 {
  public static void main(String[] args) {
    (new Object3()).method(10);
  }

  class A {
    int a1;
  }

  A f1 = new A();
  A f2 = new A();

  int method(int x) {
    if (x > 20)
      f1.a1 = 10;
    if (x > 30)
      f2.a1 = 20;
    return f1.a1 + f2.a1;
  }

}
