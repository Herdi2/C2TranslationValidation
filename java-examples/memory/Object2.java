class Object2 {
  public static void main(String[] args) {
    (new Object2()).method(10);
  }

  class A {
    int a1;

    public A(int a1) {
      this.a1 = a1;
    }
  }

  A f1 = new A(10);

  int method(int x) {
    return f1.a1;
  }
}
