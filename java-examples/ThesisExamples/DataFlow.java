class DataFlow {
  public static void main(String[] args) {
    DataFlow df = new DataFlow();
    df.dataflow(10, 12);
  }

  int dataflow(int x, int y) {
      int a = x - y;
      int b = x + y;
      return a + b;
  }

}
