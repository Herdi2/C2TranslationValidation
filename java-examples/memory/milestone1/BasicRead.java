class BasicRead {
  public static void main(String[] args) {
    (new BasicRead()).method(10);
  }

  int f1 = 10;

  int method(int x) {
    return f1 + x;
  }

}
