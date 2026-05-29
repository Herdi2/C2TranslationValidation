class IfSubsuming1 {
  public static void main(String[] args) {
    IfSubsuming1 i = new IfSubsuming1();
    System.out.println(i.method(2130707455, -2130707456));
  }

  int method(int x, int y) {
    // In C2 change so le+F/ne is true, i.e. if x <= y then we
    // incorrectly optimize x != y to always take the true branch (should be false)
    // ControlBugs=10
    // We find this bug!
    if (x > y) {
      if (x == y) {
        return 1;
      } else {
        return 2;
      }
    } else {
      return 3;
    }
  }

}

