

int main () {
  int res;
  try {
    res = fct(10);
    if(res) printf("OK with %d\n", res);
  } catch(E e) {
    printf("ERROR\n");
  }
  return 0;
}

int fct(int i) {
  try {
    try {
      try {
        throw B(i);
      } catch (C c) {
        printf("C = %d\n", c);
      } catch (D d) {
        printf("D = %d\n", d);
      } finally {
        printf("FIN 2\n");
        //return 15;
      }
    } catch (A a) {
      printf("A = %d\n", a);
    } catch (B b) {
      printf("B = %d\n", b);
      if (b > 0) fct(b - 1);
    } finally {
      printf("FIN 1\n");
      return 12;
    }
  } catch(E e) {

  } finally {
    printf ("FIN 3\n");
    //return 36;
  }
}
