class Memory1 {
  public static void main(String[] args) {
    Memory1 obj = new Memory1();
    for (int i = 0; i < 20_000; i++) {
      obj.method(i % 2);
    }
  }

  int f1 = 20;
  int f2 = 30;
  int f3 = 40;

  // Simple example of basic writing and reading to static fields
  int method(int x) {
    return f1 + f2 + f3;
  }
}
