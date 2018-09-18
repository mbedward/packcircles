#define STRICT_R_HEADERS
#include <Rcpp.h>
using namespace Rcpp;


// Forward declarations
//
bool almostZero(double x);

bool gtZero(double x);

double ordinate(double x, double lo, double hi, bool wrap);

double wrapOrdinate(double x, double lo, double hi);

int do_repulsion(NumericMatrix xyr, NumericVector weights, int c0, int c1, 
                 double xmin, double xmax, double ymin, double ymax, bool wrap);


// Attempts to position circles without overlap.
// 
// Given an input matrix of circle positions and sizes, attempts to position them
// without overlap by iterating the pair-repulsion algorithm.
// 
// @param xyr 3 column matrix (centre x, centre y, radius)
// @param weights vector of double values between 0 and 1, used as multiplicative
//   weights for the distance a circle will move with pair-repulsion.
// @param xmin lower X bound
// @param xmax upper X bound
// @param ymin lower Y bound
// @param ymax upper Y bound
// @param maxiter maximum number of iterations
// @param wrap true to allow coordinate wrapping across opposite bounds 
//
// @return the number of iterations performed.
// 
// [[Rcpp::export]]
int iterate_layout(NumericMatrix xyr, 
                   NumericVector weights,
                   double xmin, double xmax, 
                   double ymin, double ymax,
                   int maxiter,
                   bool wrap) {
                     
  int rows = xyr.nrow();
  int iter;
  
  for (iter = 0; iter < maxiter; iter++) {
    int moved = 0;
    for (int i = 0; i < rows-1; ++i) {
      for (int j = i+1; j < rows; ++j) {
        if (do_repulsion(xyr, weights, i, j, xmin, xmax, ymin, ymax, wrap)) {
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
 * wrap    - allow coordinate wrapping across opposite bounds
 */
int do_repulsion(NumericMatrix xyr, 
                 NumericVector weights,
                 int c0, int c1,
                 double xmin, double xmax, 
                 double ymin, double ymax,
                 bool wrap) {
                   
    // if both weights are zero, return zero to indicate
    // no movement
    if (almostZero(weights[c0]) && almostZero(weights[c1])) return 0;
    
    double dx = xyr(c1, 0) - xyr(c0, 0);
    double dy = xyr(c1, 1) - xyr(c0, 1);
    double d = sqrt(dx*dx + dy*dy);
    double r = xyr(c1, 2) + xyr(c0, 2);
    double p, w0, w1;
 
    if (gtZero(r - d)) {
      if (almostZero(d)) {
        // The two centres are coincident or almost so.
        // Arbitrarily move along x-axis
        p = 1.0;
        dx = r - d;
      } else {
        p = (r - d) / d;
      }

      w0 = weights[c0] * xyr(c1, 2) / r;
      w1 = weights[c1] * xyr(c0, 2) / r;
      
      xyr(c1, 0) = ordinate( xyr(c1, 0) + p*dx*w1, xmin, xmax, wrap );
      xyr(c1, 1) = ordinate( xyr(c1, 1) + p*dy*w1, ymin, ymax, wrap );
      xyr(c0, 0) = ordinate( xyr(c0, 0) - p*dx*w0, xmin, xmax, wrap );
      xyr(c0, 1) = ordinate( xyr(c0, 1) - p*dy*w0, ymin, ymax, wrap );
      
      return(1);
    }
    
    return(0);
}

/*
 * Adjust an X or Y ordinate to the given bounds by either wrapping
 * (if `wrap` is true) or clamping (if `wrap` is false).
 */
double ordinate(double x, double lo, double hi, bool wrap) {
  if (wrap) return wrapOrdinate(x, lo, hi);
  else return std::max(lo, std::min(hi, x));
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

bool almostZero(double x) {
  static double TOL = 0.00001;
  
  return std::abs(x) < TOL;
}

bool gtZero(double x) {
  return !almostZero(x) && (x > 0.0);  
}
