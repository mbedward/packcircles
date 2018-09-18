#define STRICT_R_HEADERS
#include <Rcpp.h>
using namespace Rcpp;

#include <map>
#include <vector>
#include <float.h>

using namespace std;

// Class to generate random integers using 
// R runif in the background.
class RandomInts {
public:
  const static int NStore = 1000;
  
  RandomInts() {
    store = runif(NStore);
    pos = 0;
  }
  
  int nextInt(int max) {
    int r = (int)(store[pos] * (max + 1));
    increment_pos();
    return r;
  }
  
private:
  int pos;
  NumericVector store;
  
  void increment_pos() {
    pos += 1;
    if (pos >= NStore) {
      store = runif(NStore);
      pos = 0;
    }
  }
};

RandomInts RANDOM;


// State values for circles
const int Selected = 1;
const int Candidate = 0;
const int Rejected = -1;


// Options for order of removal
const StringVector OrderingLabels = StringVector::create(
  "maxov", "minov", "largest", "smallest", "random"
);

enum OrderingCodes {
  ORDER_MAXOV,
  ORDER_MINOV,
  ORDER_LARGEST,
  ORDER_SMALLEST,
  ORDER_RANDOM
};


class Circle {
public:
  Circle(double x_, double y_, double r_) : 
    x(x_), y(y_), radius(r_), state(Candidate) {}
  
  bool intersects(const Circle& other, double tolerance) {
    double dx = x - other.x;
    double dy = y - other.y;
    double rsum = radius + other.radius;
    
    return (dx*dx + dy*dy < rsum * rsum * tolerance);    
  }
  
  double x;
  double y;
  double radius;
  int state;
};


class Circles {
public:
  Circles(NumericMatrix xyr, double tolerance) {
    const int N = xyr.nrow();
    
    for (int i = 0; i < N; i++) {
      _circles.push_back( Circle(xyr(i, 0), xyr(i, 1), xyr(i, 2)) );
      _neighbours.push_back( vector<int>() );
    }
    
    // Record overlaps for each circle in the initial configuration
    for (int i = 0; i < N-1; i++) {
      Circle& ci = _circles.at(i);
      
      for (int j = i+1; j < N; j++) {
        Circle& cj = _circles.at(j);
        
        if (ci.intersects(cj, tolerance)) {
          _neighbours.at(i).push_back(j);
          _neighbours.at(j).push_back(i);
        }
      }
    }
  }
  
  
  // Finds a subset of non-overlapping circles
  LogicalVector select_circles(const int ordering) {
    const int N = _circles.size();
    int ndone = 0;
    
    while (ndone < N) {
      IntegerVector nbrCount(N, 0);
      for (int i = 0; i < N; i++) {
        Circle& ci = _circles.at(i);
        
        if (ci.state == Candidate) {
          nbrCount[i] = count_neighbours(i);
          
          if (nbrCount[i] == 0) {
            ci.state = Selected;
            ndone++ ;
          }
        }
      }
      
      if (ndone < N) {
        // Find circle(s) with current min or max number of overlaps,
        // depending on the argument bigFirst, and randomly choose one
        // for removal
        LogicalVector b(nbrCount.length());
        switch (ordering) {
        case ORDER_MAXOV:
          b = nbrCount == max(nbrCount); break;
          
        case ORDER_MINOV:
        {
          IntegerVector x = nbrCount[nbrCount > 0];
          int val = min(x);
          b = nbrCount == val;
        }
        break;

        case ORDER_LARGEST:
          b = flag_largest(nbrCount > 0); break;
          
        case ORDER_SMALLEST:
          b = flag_smallest(nbrCount > 0); break;
          
        case ORDER_RANDOM:
          b = nbrCount > 0; break;
        }
          
        IntegerVector ids = which(b);
        int removeId = sample_one_of(ids);
        
        _circles.at(removeId).state = Rejected;
        
        ndone++ ;
      }
    }
     
    LogicalVector sel(N, false);
    for (int i = 0; i < N; i++) {
      sel[i] = _circles.at(i).state == Selected;
    }
    
    return sel;
  }

  
private:    
  // Count Candidate neighbours of circle id.
  int count_neighbours(int id) {
    int n = 0;
    
    const vector<int>& nbrs = _neighbours.at(id);
    
    if (!nbrs.empty()) {
      for (unsigned int k = 0; k < nbrs.size(); k++) {
        int nbrId = nbrs.at(k);
        if (_circles.at(nbrId).state == Candidate) n++ ;
      }
    }
    
    return  n;
  }
  
  
  // helper function - returns a logical vector indicating which
  // circles are included and largest amongst included
  LogicalVector flag_largest(const LogicalVector& include) {
    NumericVector radii(_circles.size(), 0.0);
    for (unsigned int i = 0; i < _circles.size(); i++) {
      if (include[i]) {
        radii[i] = _circles.at(i).radius;
      }
    }
    
    return radii == max(radii);  
  }
  
  
  // helper function - returns a logical vector indicating which
  // circles are included and largest amongst included
  LogicalVector flag_smallest(const LogicalVector& include) {
    NumericVector radii(_circles.size(), DBL_MAX);
    for (unsigned int i = 0; i < _circles.size(); i++) {
      if (include[i]) {
        radii[i] = _circles.at(i).radius;
      }
    }
    
    return radii == min(radii);
  }
  
  
  // helper function - gets indices of elements
  // in a LogicalVector that are true
  IntegerVector which(const LogicalVector& b) {
    IntegerVector ii = Range(0, b.length() - 1);
    return ii[b];
  }
  
  
  // helper function - select a random element
  // from x
  int sample_one_of(const IntegerVector& x) {
    int n = x.length();
    
    if (n < 2) return x[0];
    else {
      int i = RANDOM.nextInt(n-1);
      return x[i];
    }
  }

    
  vector<Circle> _circles;
  vector< vector<int> > _neighbours;
};



// Function called from R.
//
// Takes a set of circles, each defined by centre xy coordinates
// and radius, and iteratively selects those with no overlaps and
// discards a random chosen one from those with the most overlaps.
//
// Returns a logical vector with selected = true.
//
// [[Rcpp::export]]
LogicalVector select_non_overlapping(NumericMatrix xyr, 
                                     const double tolerance, 
                                     const StringVector& ordering) {
  
  int match = -1;
  try {
    for (int i = 0; i < OrderingLabels.length(); i++) {
      if ( OrderingLabels[i] == ordering[0] ) {
        match = i;
        break;
      }
    }
    
    if (match >= 0) {
      Circles cs(xyr, tolerance);
      return cs.select_circles(match);
    }
    else throw std::invalid_argument("Invalid ordering argument");
    
  } catch (std::exception& ex) {
    forward_exception_to_r(ex);
  }
  
  return NA_LOGICAL;  // not reached
}

