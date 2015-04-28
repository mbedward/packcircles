#include <Rcpp.h>
using namespace Rcpp;

// Forward declarations
//
double toToroid(double ord, double lim);
int do_repulsion(NumericMatrix xyr, int c0, int c1, double xbound, double ybound);


//' Attempts to position circles without overlap.
//' 
//' Given an input matrix of circle positions and sizes, attempts to position them
//' without overlap by iterating the pair-repulsion algorithm.
//' 
//' @param xyr 3 column matrix (centre x, centre y, radius)
//' @param xbound limit in X direction
//' @param ybound limit in Y direction
//' @param maxiter maximum number of iterations
//' 
//' @return the number of iterations performed.
//' 
int iterate_layout(NumericMatrix xyr, double xbound, double ybound, int maxiter) {
  int rows = xyr.nrow();
  int iter;
  
  for (iter = 0; iter < maxiter; iter++) {
    int moved = 0;
    for (int i = 0; i < rows-1; ++i) {
      for (int j = i+1; j < rows; ++j) {
        if (do_repulsion(xyr, i, j, xbound, ybound)) {
          moved = 1;
        }
      }
    }
    if (!moved) break;
  }
  
  return iter;
}


/*
 * Checks if two circles overlap excessively and, if so, moves them
 * apart. The distance moved by each circle is proportional to the
 * radius of the other to give some semblance of intertia.
 * 
 * xyr     - 3 column matrix of circle positions and sizes (x, y, radius)
 * c0      - index of first circle
 * c1      - index of second circle
 * xbound  - limit in X direction
 * ybound  - limit in Y direction
 */
int do_repulsion(NumericMatrix xyr, int c0, int c1, double xbound, double ybound) {
    static double REPEL_TOL = 0.0001;
    
    double dx = xyr(c1, 0) - xyr(c0, 0);
    double dy = xyr(c1, 1) - xyr(c0, 1);
    double d = sqrt(dx*dx + dy*dy);
    double r = xyr(c1, 2) + xyr(c0, 2);
    double p, w0, w1;

    if (d < r - REPEL_TOL) {
      if (d < REPEL_TOL) {
        // arbitrarily move along x-axis
        p = 1.0;
        dx = r - d;
      } else {
        p = (r - d) / d;
      }

      w0 = xyr(c1, 2) / r;
      w1 = xyr(c0, 2) / r;

      xyr(c1, 0) = toToroid( xyr(c1, 0) + p*dx*w1, xbound );
      xyr(c1, 1) = toToroid( xyr(c1, 1) + p*dy*w1, ybound );
      xyr(c0, 0) = toToroid( xyr(c0, 0) - p*dx*w0, xbound );
      xyr(c0, 1) = toToroid( xyr(c0, 1) - p*dy*w0, ybound );

      return(1);
    }

    return(0);
}


/*
 * Map an X or Y ordinate to the toroidal interval [0, lim).
 *
 * ord - X or Y ordinate to be adjusted
 * lim - bounding ordinate
 */
double toToroid(double ord, double lim) {
  while (ord < 0.0) ord += lim;
  while (ord >= lim) ord -= lim;
  return ord;
}
