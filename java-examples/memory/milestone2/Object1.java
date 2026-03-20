class Object1 {
  public static void main(String[] args) {
    (new Object1()).method(1);
  }

  class A {
    int a1 = 20;
    int a2 = 30;
  }

  A f1 = new A();

  public int method(int x) {
    return f1.a1 + x;
  }
}
