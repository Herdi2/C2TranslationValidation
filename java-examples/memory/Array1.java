class Array1 {
  public static void main(String[] args) {
    (new Array1()).method(1, 2);
  }

  int arr[] = new int[4];

  int method(int x, int y) {
    return arr[0] + arr[x];
  }
}
