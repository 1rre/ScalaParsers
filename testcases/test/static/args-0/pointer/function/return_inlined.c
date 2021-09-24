typedef int(*rtnFn)(int);

int do_call(rtnFn f) {
 return f(5);
}

int f() {
  int y = 5;
  int addOneTo(int x) {
    return y + x;
  }
  return do_call(addOneTo);
}