public class MulAsso {
  public static void main(String[] args) {
    float f = method(8.635234f);
    System.out.println(f);
  }

  static float method(float x) {
    float v1 = 0.123f;
    float v2 = 1.23f;
    return (x * v1) * v2;
  }
}
