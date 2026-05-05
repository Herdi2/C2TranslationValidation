class IfEquality1 {
  public static void main(String[] args) {
    IfEquality1 ie = new IfEquality1();
    System.out.println(ie.method(1));
  }

  int method(int x) {
    int res = 0;

    // ControlBugs=20, incorrectly removes x > 1 since
    // we remove the same_condition check
    // Our tools finds a bug when x = 1.
    // ????? The generated XML does not contain this bug btw
    if (x > 0) {
      res += 1;
      if (x > 1) {
        res += 10;
      }
    }

    return res;
  }

}
