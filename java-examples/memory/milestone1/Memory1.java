class Memory1 {
  public static void main(String[] args) {
    (new Memory1()).method(10);
  }

  int f1 = 20;
  int f2 = 30;
  int f3 = 40;

  // Simple example of basic writing and reading to static fields
  int method(int x) {
    f1 = x;
    f2 = x;
    return f3;
  }
}
