#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix NewTip(NumericMatrix x, int parent, int child, double branch) {
  int nrow = x.nrow();
  NumericMatrix::Column z = x(_, 0);
  int pos = 0;
  for(int i = 0; i < nrow; i++) {
    if(NumericVector::is_na(z[i])) {
      pos = i;
      break;
    }
  }
  int father = 0;
  NumericMatrix::Column y = x(_, 2);
  LogicalVector mY = y == parent;
  for(int i = 0; i < nrow; i++) {
    if(!NumericVector::is_na(y[i])) {
      if(mY[i]) {
        father = i;
        break;
      }
    }
  }
  NumericMatrix::Column w = x(_, 1);
  int m = max(na_omit(w)) + 1;
  int pos2 = pos + 1;
  x(pos, 0) = x(father, 1);
  x(pos2, 0) = x(father, 1);
  x(pos, 1) = m;
  int m2 = m + 1;
  x(pos2, 1) = m2;
  x(pos, 2) = parent;
  x(pos2, 2) = child;
  x(father, 2) = NA_REAL;
  x(pos, 3) = branch;
  x(pos2, 3) = branch;
  for(int i = 1; i < m2 & i != pos & i != pos2; i++){
    if(y[i] > -1) {
      x(i, 3) = x(i, 3) + branch;
    }
  }
  return x;
}
