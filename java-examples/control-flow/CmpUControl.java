class CmpUControl {
  public static void main(String[] args) {
    CmpUControl c = new CmpUControl();
      try {
        System.out.println(c.method(false));
      } catch (Exception e) {
        System.out.println(e.toString());
      }
  }

  int one = 128082026;
  int method(boolean flag) {
      int minimum, maximum;
      if (flag) {
          minimum = 0;
          maximum = 1;
      } else {
          // Always goes to else-path
          minimum = Integer.MIN_VALUE;
          maximum = Integer.MAX_VALUE;
      }
      // one < INT_MIN || one > MAX_INT
      // 1 < INT_MIN    || 1 > MAX_INT
      //    false          false
      // => false
      //
      // C2 transforms this into:
      // one - minimum >=u (maximum - minimum) + 1
      // 1    - INT_MIN >=u (INT_MAX - INT_MIN) + 1
      // INT_MIN + 1    >=u -1                  + 1
      // INT_MIN + 1    >=u 0
      // => true
      if (one < minimum || one > maximum) {
          throw new RuntimeException();
      }
      return 0;
  }
}
