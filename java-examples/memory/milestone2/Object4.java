class Object4 {

  public static void main(String[] args) {
    Object4 obj = new Object4();
    obj.method(10, 20);
  }

  class A {
    int x;
  }

  A f1 = new A();
  A f2 = new A();

  public int method(int x, int y) {
    if (f1 != null) {
      return f1.x;
    } else {
      return f2.x;
    }
  }
}
