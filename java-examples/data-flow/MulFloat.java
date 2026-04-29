public class MulFloat {
  public static void main(String[] args) {
    float f = method(-0.0f);
    System.out.println(f);
  }

  static float method(float x) {
    return 0.0f * x;
  }
}
