class SameStoreSameValue {
  public static void main(String[] args) {
    SameStoreSameValue sv = new SameStoreSameValue();
    System.out.println(sv.correct(10));
    System.out.println(sv.incorrect(10));
    System.out.println(sv.incorrect2(10));
  }

  class A {int a;}
  A f1 = new A();
  A f2 = new A();

  int correct(int x) {
    // `StoreNode::Identity` removes two stores in a row
    // when they contain the same value and write to the same address.
    f1.a = 10;
    f1.a = 10; // Gets removed!
    return f1.a;
  }

  int incorrect(int x) {
    // `StoreNode::Identity` removes two stores in a row
    // when they contain the same value and write to the same address.
    // Non-trivial mistake would be to compare addresses by value, not by node.
    // MemoryBugs=30
    f2.a = 10;
    f1.a = 10; // Gets removed!
    return f1.a;
  }

  int incorrect2(int x) {
    // `StoreNode::Identity` removes two stores in a row
    // when they contain the same value and write to the same address.
    // Non-trivial mistake would be to compare addresses by value, not by node.
    // MemoryBugs=30
    f1.a = 10;
    f1.a = 11; // Gets removed!
    return f1.a;
  }

}
