class Test {
  public static void main(String[] args) {
    try {
      double checksum = method(-1243682091);
      System.out.println(checksum);
    } catch (Exception e) {
      System.out.println(e);
    }
  }

  static double method(int v_0) {
    int v_1 = (v_0) / (1243682086);
    return (double) v_1;
  }
}
