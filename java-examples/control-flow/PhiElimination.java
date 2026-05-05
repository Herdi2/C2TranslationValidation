class PhiElimination {
  public static void main(String[] args) {
    PhiElimination p = new PhiElimination();
    System.out.println(p.correct(10));
    System.out.println(p.incorrect(10));
  }
  
  int correct(int n) {
    // Dead branches into a region (i.e. cannot be reached)
    // will be marked as such and removed from the phi.
    // If this is done incorrectly, we have a bug!
    // Due to complexity in marking a branch as dead,
    // this will be done externally.
    int res = n;
    if (n > 1) {
      res += 80;
    } else if (n < 1) {
      res += 60;
    } else {
      res *= 2;
    }
    return res;
  }
  
  int incorrect(int n) {
    // Dead branches into a region (i.e. cannot be reached)
    // will be marked as such and removed from the phi.
    // If this is done incorrectly, we have a bug!
    // Due to complexity in marking a branch as dead,
    // this will be done externally.
    int res = n;
    if (n > 1) {
      res += 80;
    } else if (n <= 1) {
      res += 60;
    } else {
      res *= 2;
    }
    return res;
  }


}
