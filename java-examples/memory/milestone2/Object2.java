class Object2 {
  public static void main(String[] args) {
    Object2 obj = new Object2();
    System.out.println(obj.method(false, obj.new A(1, 2)));
  }

  class A {
    int a1;
    int a2;

    public A(int a1, int a2) {
      this.a1 = a1;
      this.a2 = a2;
    }
  }

  A f1 = new A(3, 4);

  public int method(boolean flag, A a) {
    if (flag) {
      f1 = a;
    }
    a.a1 = 100;
    return f1.a1;
  }
}
