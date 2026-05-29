class ControlFlow {
  public static void main(String[] args) {
    ControlFlow cf = new ControlFlow();
    cf.compare(10, 11);
  }

  int compare(int x, int y) {
    if (x > y) {
      return 1;
    } else if (x < y) {
      return -1;
    } else {
      return 0;
    }
  }
}
