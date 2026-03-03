public class AndNeg{
    public static void main(String[] args) {
        int x = andneg(-3, -4);
        System.out.println(x);
    }

    static int andneg(int x, int y){
        return (-x) & (-y);
    }
}
