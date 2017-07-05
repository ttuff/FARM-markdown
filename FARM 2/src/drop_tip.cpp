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
NumericVector DropTip(NumericMatrix x, NumericVector extinct) {
  int n = extinct.size();
  int nrow = x.nrow();
  int ncol = x.ncol();
  for(int k = 0; k < n; k++) {
    int target = 0;
    for(int j = 0; j < nrow; j++) {
      if(extinct[k] == x(j, 2)) {
        target = j;
        break;
      }
    }
    int trans = x(target, 0);
    NumericMatrix::Row w = x(target, _);
    for(int i = 0; i < ncol; i++) {
      w[i] = NA_REAL;
    }
    int target2 = 0;
    for(int i = 0; i < nrow; i++) {
      if(x(i, 0) == trans) {
        target2 = i;
        break;
      }
    }
    int target3 = 0;
    for(int i = 0; i < nrow; i++) {
      if(x(i, 1) == trans) {
        target3 = i;
        break;
      }
    }
    x(target3, 1) = x(target2, 1);
    x(target3, 3) = x(target2, 3) + x(target3, 3);
    x(target3, 2) = x(target2, 2);
    NumericMatrix::Row w2 = x(target2, _);
    for(int i = 0; i < ncol; i++) {
      w2[i] = NA_REAL;
    }
  }
  NumericMatrix::Column w3 = x(_, 0);
  int y = -1;
  NumericVector pos2(nrow);
  for(int i = 0; i < nrow; i++) {
    if(!NumericVector::is_na(w3[i])) {
      y += 1;
      pos2[y] = i;
    }
  }
  y += 1;
  for(int i = 0; i < y; i++) {
    for(int l = 0; l < ncol; l++) {
      x(i, l) = x(pos2[i], l);
    }
  }
  for(int i = y; i < nrow; i++) {
    for(int l = 0; l < ncol; l++) {
      x(i, l) = NA_REAL;
    }
  }
  return x;
}
