class IfEquality2 {
  public static void main(String[] args) {
    IfEquality2 ie = new IfEquality2();
    System.out.println(ie.method(-3));
  }

  int method(int x) {
    int res = 0;
    // ControlBugs=21, 
    if (x < 5) {
      res += 10;
      x += 10;
    }
    if (x < 5) {
      res += 20;
    }

    return res;
  }

}
