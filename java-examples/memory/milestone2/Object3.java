class Object3 {
  public static void main(String[] args) {
    (new Object3()).method(false);
  }

  class A {
    int a1 = 20;
    int a2 = 30;
  }

  A f1 = new A();

  public int method(boolean flag) {
    if (flag) {
      f1 = null;
    }
    return f1.a1;
  }
}
