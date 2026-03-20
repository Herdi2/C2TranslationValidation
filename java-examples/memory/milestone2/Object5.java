class Object5 {

  public static void main(String[] args) {
    Object5 obj = new Object5();
    obj.method(10, 20);
  }

  class A {
    int a1;
  }

  A f1 = new A();
  A f2 = new A();

  public int method(int x, int y) {
    if (f1 == f2) {
      return x;
    } else {
      return y;
    }
  }
}
