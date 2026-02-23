public class MulAsso {
  public static void main(String[] args) {
    float f = mulasso(1.800018061301167e-29f);
    System.out.println(f);
  }

  static float mulasso(float x) {
    float v1 = 0.123f;
    float v2 = 1.23f;
    return (x * v1) * v2;
  }
}
