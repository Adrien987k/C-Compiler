

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
      try {
        return 8;
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
      return 9;
      printf("B = %d\n", b);
    } finally {
      printf("FIN 1\n");
      //return 12;
    }
  } catch(E e) {

  } finally {
    printf ("FIN 3\n");
    //return 36;
  }
}
