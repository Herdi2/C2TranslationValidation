class IfSubsuming2 {
  public static void main(String[] args) {
    IfSubsuming2 i = new IfSubsuming2();
    System.out.println(i.method(2130707455, -2130707456));
  }

  int method(int x, int y) {
    // We implement an incorrect subsumption optimization
    // where we assume x >= y subsumes x > y, which it does not.
    // ControlBugs=11
    // We find this bug!
    if (x <= y) {
      if (x < y) {
        return 1;
      } else {
        return 2;
      }
    } else {
      return 3;
    }
  }

}

