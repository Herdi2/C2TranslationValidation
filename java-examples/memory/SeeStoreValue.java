class SeeStoreValue {
  public static void main(String[] args) {
    SeeStoreValue s = new SeeStoreValue();
    System.out.println(s.correct());
    System.out.println(s.incorrect());
  }

  class A {int a;}
  A f1 = new A();
  A f2 = new A();

  int correct() {
    f1.a = 10;
    return f1.a;
  }

  int incorrect() {
    f1.a = 10;
    f2.a = 20;
    return f1.a;
  }

}
