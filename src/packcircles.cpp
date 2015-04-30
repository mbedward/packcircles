#include <Rcpp.h>
using namespace Rcpp;

// Forward declarations
//
double wrapOrdinate(double x, double lo, double hi);

int do_repulsion(NumericMatrix xyr, int c0, int c1, 
                 double xmin, double xmax, double ymin, double ymax);


// Attempts to position circles without overlap.
// 
// Given an input matrix of circle positions and sizes, attempts to position them
// without overlap by iterating the pair-repulsion algorithm.
// 
// @param xyr 3 column matrix (centre x, centre y, radius)
// @param xmin lower X bound
// @param xmax upper X bound
// @param ymin lower Y bound
// @param ymax upper Y bound
// @param maxiter maximum number of iterations
// 
// @return the number of iterations performed.
// 
// [[Rcpp::export]]
int iterate_layout(NumericMatrix xyr, 
                   double xmin, double xmax, 
                   double ymin, double ymax,
                   int maxiter) {
                     
  int rows = xyr.nrow();
  int iter;
  
  for (iter = 0; iter < maxiter; iter++) {
    int moved = 0;
    for (int i = 0; i < rows-1; ++i) {
      for (int j = i+1; j < rows; ++j) {
        if (do_repulsion(xyr, i, j, xmin, xmax, ymin, ymax)) {
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
 * xmin    - bounds min X
 * xmax    - bounds max X
 * ymin    - bounds min Y
 * ymax    - bounds max Y
 */
int do_repulsion(NumericMatrix xyr, 
                 int c0, int c1, 
                 double xmin, double xmax, 
                 double ymin, double ymax) {
                   
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

      xyr(c1, 0) = wrapOrdinate( xyr(c1, 0) + p*dx*w1, xmin, xmax );
      xyr(c1, 1) = wrapOrdinate( xyr(c1, 1) + p*dy*w1, ymin, ymax );
      xyr(c0, 0) = wrapOrdinate( xyr(c0, 0) - p*dx*w0, xmin, xmax );
      xyr(c0, 1) = wrapOrdinate( xyr(c0, 1) - p*dy*w0, ymin, ymax );

      return(1);
    }

    return(0);
}


/*
 * Map an X or Y ordinate to the toroidal interval [lo, hi).
 *
 * x  - X or Y ordinate to be adjusted
 * lo - lower coordinate bound
 * hi - upper coordinate bound
 */
double wrapOrdinate(double x, double lo, double hi) {
  double w = hi - lo;
  while (x < lo) x += w;
  while (x >= hi) x -= w;
  return x;
}
