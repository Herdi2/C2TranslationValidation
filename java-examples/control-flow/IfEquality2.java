class IfEquality2 {
  public static void main(String[] args) {
    IfEquality2 ie = new IfEquality2();
    System.out.println(ie.method(3));
  }

  int f1;
  int method(int x) {
    // ControlBugs=21, 
    int res = 0;
    if (x > 1) {
      if (x > 2) {
        x += 10;
      }
      if (x > 2) {
        x += 20;
      }
    }

    return x;
  }

}
