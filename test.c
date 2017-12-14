
int main () {
  printf("main: depth = %d\n", depth);
  try {
    function();
    throw F("");
  } catch (B e) {
    printf("B\n");
    return 5;
  } catch (F f) {
    printf("F\n");
    printf("main: depth = %d\n", depth);
    return 2;
  }
  printf("main: depth = %d\n", depth);
  printf("End\n");
  return 0;
}

int function () {
  printf("function: depth = %d\n", depth);
  function2();
  printf("function: depth = %d\n", depth);
  return;
}

int function2 () {
  printf("function2: depth = %d\n", depth);
  //throw F("");
  return;
}

