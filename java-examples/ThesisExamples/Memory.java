class Memory {
  public static void main(String[] args) {
    Memory m = new Memory();
    m.memory();
  }

  int f1;
  int f2;

  int memory() {
    f1 = 1;
    f1 = 2;
    f2 = 3;
    f2 = 4;
    return f1 + f2;
  }

}
