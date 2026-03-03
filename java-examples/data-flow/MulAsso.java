public class MulAsso {
  public static void main(String[] args) {
    float f = mulasso(2.325661e15f);
    System.out.println(f);
  }

  static float mulasso(float x) {
    float v1 = 0.123f;
    float v2 = 1.23f;
    return (x * v1) * v2;
  }
}
