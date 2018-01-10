

int main () {
  int res;
  try {
    res = fct();
    if(res) printf("OK with %d\n", res);
  } catch(E e) {
    printf("ERROR\n");
  }
  return 0;
}

int fct() {
  try {
    try {
      return 8;
    } catch (C c) {
      printf("C = %d\n", c);
    } catch (D d) {
      printf("D = %d\n", d);
    } finally {
      return 15;
      printf("FIN 2\n");
    }
  } catch (A a) {
      printf("A = %d\n", a);
  } catch (B b) {
      return 9;
      printf("B = %d\n", b);
  } finally {
    return 12;
    printf("FIN 1\n");
  }
}
