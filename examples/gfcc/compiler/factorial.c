int fact (int n) {
  int f ;
  f = 1 ;
  {
  while (1 < n) {
    f = n * f ;
    n = n - 1 ;
  }
  }
  return f ;
} ;

int main () {
  int n ;
  n = 1 ;
  {
    while (n < 11) printf(int,fact(n)) ; n = n+1 ;
  }
  return ;
} ;

