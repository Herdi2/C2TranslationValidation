public class Floating {
    public static void main(String[] args) {
        float f = floating(-0.0f);
        System.out.println(f);
    }

    static float floating(float f) {
        return 0 - (0 - f);
    }

}