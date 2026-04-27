class Test {
  public static void main(String[] args) {
    Test t = new Test();
    for (int i = -20_000; i < 20_000; i++) {
      t.method(i);
      t.ifguardsubsuming(i);
      t.ifequality(i);
      t.phielimination(i);
      t.diamond(i);
      t.rangecheck(i);
      t.back2backstores(i);
      t.reuseloads(i);
      t.removingstores(i);
    }
  }

  int ifguardsubsuming(int x) {
    int res = 0;
    if (x > 1) {
      if (x < 0) {
        res = 1;
      } else {
        res += 2;
      }
    } else {
      res = 3 - x;
    }
    return res;
  }

  int ifequality(int x) {
    int res = 0;

    if (x > 0) {
      res += 1;
      f3 += 1;
    }

    if (x > 0) {
      res += 1;
      f3 += 1;
    }

    return res;
  }

  int phielimination(int x) {
    int res = 0;
    if (x - x != 0) {
      res += 1;
    } else if (x - x + 1 != 1) {
      res += 2;
    } else {
      res += 3;
    }

    return res;
  }

  int diamond(int x) {
    if (x > 0) {
      return 0;
    } else {
      return 1;
    }
  }

  int rangecheck(int x) {
    int res = x;
    if (x > 10 && x < 20) {
      res += 10;
    } else {
      res += 20;
    }
    return res;
  }

  int back2backstores(int x) {
    if (x > 0) {
      f3 = 1;
      f3 = 2;
    }
    return f3;
  }

  int reuseloads(int x) {
    return x + f3 + f3;
  }

  int removingstores(int x) {
    if (x > 0) {
      f3 = x;
    }
    return f3;
  }

  class A {
    int x;
  }

  A f1 = new A();
  A f2 = new A();
  int f3 = 0;

  int method(int x) {
    if (x >= 0) {
      if (x <= 0) {
        return 0;
      } else {
        return 1;
      }
    } else {
      return 2;
    }
  }

  int method2(int x) {
    int res = 0;
    int x2 = x;
    if (x - x != 0) {
      res = 10;
    } else if (x - x2 != 0) {
      res = 20;
    } else {
      res = 30;
    }
    int y = res * 2 + x;
    return y;
  }
}
