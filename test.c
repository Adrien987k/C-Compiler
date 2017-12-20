
int main () {
  try {
    function();
    throw F("tata");
  } catch (F f) {
    printf("F = %s\n", f);
    return 4;
  } catch (B b) {
    printf("B\n");
  } finally {
    printf("Finally\n");
    return 5;
  }
  return 0;
}


int function () {
  function2();
  return 5;
}

int function2 () {
  throw F("toto");
  return;
}
