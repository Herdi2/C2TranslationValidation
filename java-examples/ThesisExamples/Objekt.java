
class Objekt {
  public static void main(String[] args) {
    Objekt obj = new Objekt();
    obj.objects();
  }

  class A {int x;}
  A f1 = new A();
  A f2 = new A();

  int objects() {
    f1.x = 10;
    f2.x = 20;
    return f1.x * f2.x;
  }
}
