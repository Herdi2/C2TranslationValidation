class IfCanonical {
  public static void main(String[] args) {
    IfCanonical i = new IfCanonical();
    System.out.println(i.method(10));
  }

  int method(int x) {
    int res = 10;
    // Comparison will be canonicalized to x < 0.
    // Bug possible if branches are not switched.
    if (x >= 0) {
      res = 10;
    } else {
      res += 20;
    }
    return res;
  }
}
