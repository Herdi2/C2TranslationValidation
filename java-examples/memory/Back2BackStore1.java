class Back2BackStore1 {
  public static void main(String[] args) {
    Back2BackStore1 b2b = new Back2BackStore1();
    System.out.println(b2b.correct(10));
    System.out.println(b2b.incorrect(10));
  }

  class A {int x;}
  A f1 = new A();
  A f2 = new A();
  int f3;
  int f4;

  // NOTE: To not get folded up immediately, need to use -XX:+DelayMem
  // as it will remove LoadNode::Value and LoadNode::Identity from
  // immediately returning the written value.

  int correct(int x) {
      // StoreNode::Ideal
      // Back-to-back stores to the same address are folded
    f1.x = 10;
    f1.x = 20;
    return f1.x;
  }

  int incorrect(int x) {
    // StoreNode::Ideal
    // Back-to-back store done incorrectly here, due to assuming
    // the addresses are the same!
    // NOTE: Cannot be implemented in the compiler, since
    // back-to-back stores to addresses have more than one outcnt(),
    // and changing that criteria crashes the compiler.
    f1.x = 10;
    f2.x = 20;
    return f1.x;
  }

}
