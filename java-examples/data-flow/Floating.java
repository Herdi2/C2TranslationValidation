public class Floating {
    public static void main(String[] args) {
        float f = method(-0.0f);
        System.out.println(f);
    }

    static float method(float f) {
        return 0 - (0 - f);
    }

}
