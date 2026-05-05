class LoadIntoStore {
  public static void main(String[] args) {
    LoadIntoStore lis = new LoadIntoStore();
    System.out.println(lis.correct(10));
    System.out.println(lis.incorrect(10));
  }

  class A {int a;}
  A f1 = new A();
  A f2 = new A();

  // NOTE: Once again -XX:+DelayMem is needed

  int correct(int x) {
    // Loading the same value into the same store, e.g. x = x is useless.
    // Thus, f1.a = f1.a is removed in optimizations.
    f1.a = f1.a;
    return f1.a + x;
  }

  int incorrect(int x) {
    // Loading the same value into the same store, e.g. x = x is useless.
    // Bug however, if the addresses do not match, in this case f2.a = f1.a
    // MemoryBugs=20
    f2.a = 10;
    f2.a = f1.a;
    return f2.a + x;
  }

}
