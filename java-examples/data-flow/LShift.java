public class LShift {
  public static void main(String[] args) {
    long x = method(-1);
    System.out.println(x);
  }

  static long method(long i) {
    return (i + i) << 63;
  }
}
