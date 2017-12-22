

int main () {
  try {
    if(fct()) printf("OK\n");
  } catch(E e) {
    printf("ERROR\n");
  }
  return 0;
}

int fct() {
  try {
    throw E(1);
  } finally {
    //printf("FIN\n"); //PB Car apres call printf leave ret si excp_raised=1
                       // 
    return 1;
  }
}
