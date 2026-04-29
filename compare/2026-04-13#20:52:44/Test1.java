// Generated with seed 3706427396022863538
class Test1 {
    public static void main(String[] args) {
        for (int i = 0; i < 10_000; i++) {
            try { 
                method(i, i, i);
            } catch (Exception e) {}
        }
    }

    static double method(int v_0, long v_1, long v_2) {
        int v_3 = -2074833691;
        v_0 = v_3;
        return ((double) v_3) * (((double) v_0) * (((double) v_2) + ((double) v_1)));
    }
}