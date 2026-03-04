class Memory1 {
  public static void main(String[] args) {
    method(10);
  }

  static int f1 = 20;
  static int f2 = 30;
  static int f3 = 40;

  
  // Simple example of basic writing and reading to static fields
  static int method(int x) {
    f1 = x;
    f2 = x;
    return f3;
  }
}
