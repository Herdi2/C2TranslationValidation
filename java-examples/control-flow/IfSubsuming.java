class IfSubsuming {
  public static void main(String[] args) {
    IfSubsuming i = new IfSubsuming();
    System.out.println(i.method(2130707455, -2130707456));
  }

  int method(int x, int y) {
    // In C2 change so le+F/ne is true, i.e. if x <= y then we
    // incorrectly optimize x != y to always take the true branch (should be false)
    // ControlBugs=10
    // We find this bug!
    int res = 0;
    if (x > y) {
      if (x == y) {
        res = 1;
      } else {
        res += 2;
      }
    } else {
      res = 3 - x;
    }
    return res;
  }

}
