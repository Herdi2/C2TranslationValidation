public class LShift {
  public static void main(String[] args) {
    long x = lshift(-1);
    System.out.println(x);
  }

  static long lshift(long i) {
    return (i + i) << 63;
  }
}
