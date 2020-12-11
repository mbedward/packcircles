/*
 * Implements a circle packing algorithm described in:
 *
 *   Charles R. Collins & Kenneth Stephenson (2003) A circle packing algorithm.
 *     Computation Geometry Theory and Applications 25: 233-256.
 *
 * The algorithm takes a graph which specifies a desired pattern of circle 
 * tangencies and searchs for an arrangement of circle positions and sizes 
 * which satisfy that pattern.
 *
 * This implementation is based on Python code by David Eppstein: specifically
 * the file CirclePack.py which is part of PADS (Python Algorithms and 
 * Data Structures) library, available at: https://www.ics.uci.edu/~eppstein/PADS/
 *
 * Original license header:
 *
 * PADS is licensed under the MIT Licence (https://opensource.org/licenses/MIT):
 *
 * Copyright (c) 2002-2015, David Eppstein
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 */

#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <cmath>
#include <complex>
#include <map>

using namespace Rcpp;

using std::map;
using std::pair;
using std::vector;
using std::complex;

const double Tolerance = 1.0 + 1.0e-8;

// Utility functions defined in pack_circles.cpp
bool almostZero(double x);
bool gtZero(double x);


// Tests if a map contains a given key.
template<class K, class V>
bool contains(std::map<K, V> m, K k) {
  return m.count(k) > 0;
}


// Computes the angle at a circle of radius rx given by two circles of
// radius ry and rz respectively which are tangential to circle x and
// each other.
//
double acxyz(double rx, double ry, double rz) {
  double denom = 2 * (rx + ry) * (rx + rz);
  if (almostZero(denom)) return M_PI;
  
  double num = pow(rx + ry, 2) + pow(rx + rz, 2) - pow(ry + rz, 2);
  double term = num / denom;
  
  if (term < -1.0 || term > 1.0) return M_PI / 3;
  else return acos(term);
}


// Computes the angle sum around a given internal circle.
//
double flower(const map<int, double>& radius, 
              const int center, 
              const vector<int>& cycle) {
                
  const int nc = cycle.size();
  const double rc = radius.at(center);
  double sum = 0.0;

  for (int i = 0; i < nc; i++) {
    int j = i + 1 == nc ? 0 : i + 1;
    sum += acxyz(rc, radius.at(cycle.at(i)), radius.at(cycle.at(j)));
  }
  
  return sum;
}

// Recursively find centers of all circles surrounding k.
// The placements argument is modified in place.
//
void place(map<int, complex<double> >& placements,
           const map<int, double>& radii,
           const map<int, vector<int> >& internal,
           const int centre) {
             
  // If the centre circle ID is not in the internal map
  // there is nothing to do
  if ( !contains(internal, centre) ) return;

  vector<int> cycle = internal.at(centre);  
  const int nc = cycle.size();
  const double rcentre = radii.at(centre);
    
  const complex<double> minusI = complex<double>(0.0, -1.0);
    
  for (int i = -nc; i < nc-1; i++) { // loop indices as per Python version
    int ks = i < 0 ? nc + i : i;
    int s = cycle.at(ks);
    double rs = radii.at(s);
      
    int kt = ks + 1 < nc ? ks + 1 : 0;
    int t = cycle.at(kt);
    double rt = radii.at(t);
      
    if ( contains(placements, s) && !contains(placements, t) ) {
      double theta = acxyz(rcentre, rs, rt);
      
      complex<double> offset = (placements.at(s) - placements.at(centre)) / 
          complex<double>(rs + rcentre);

      offset = offset * exp( minusI * theta );
      
      placements[t] = placements.at(centre) + offset * (rt + rcentre);
      
      place(placements, radii, internal, t);
    }
  }
}

// Finds a circle packing for the given configuration of internal and 
// external circles. 
//
// The two arguments are maps with disjoint keys (integer circle IDs).
// For the internal map, keys are internal circle IDs and values are vectors
// of neighbouring circle IDs. For the external map, keys are external circle
// IDs and values are radii.
//
// Returns a map with circle ID as key and pair<complex, double> values
// representing circle centre and radius.
//
std::map<int, pair<complex<double>, double> > CirclePack(
  map<int, vector<int> > internal,
  map<int, double> external) {
    
  // There should be no zero or negative values in external
  for (map<int, double>::iterator it = external.begin(); it != external.end(); ++it) {
    if (!gtZero(it->second)) Rcpp::stop("external radii must be positive");
  }
  
  // Copy the external map
  map <int, double> radii( external );

  for (map<int, vector<int> >::iterator it = internal.begin(); 
       it != internal.end(); ++it) {
         
    if (contains(external, it->first)) {
      std::string msg("ID found in both internal and external map keys: ");
      msg += Rcpp::toString(it->first);
      Rcpp::stop(msg);
    }
    
    radii[it->first] = 1.0;
  }
  
  // The main iteration for finding the correct set of radii
  double lastChange = Tolerance + 1;
  while (lastChange > Tolerance) {
    lastChange = 1.0;
    for (map<int, vector<int> >::iterator it = internal.begin(); 
       it != internal.end(); ++it) {
    
      int k = it->first;
      vector<int>& cycle = it->second;
      int cycleLen = cycle.size();
      
      double theta = flower(radii, k, cycle);
      double hat = radii[k] / (1.0 / sin(theta / (2*cycleLen)) - 1);
      double newrad = hat * (1.0 / sin(M_PI / cycleLen) - 1);

      double kc = std::max(newrad / radii[k], radii[k] / newrad);
      lastChange = std::max(lastChange, kc);
      
      radii[k] = newrad;
    }
  }    
    
  // Recursively place all the circles
  map<int, complex<double> > placements;
    
  int k1 = internal.begin()->first; // pick one internal circle
  placements[k1] = complex<double>(0.0);    // place it at the origin
    
  int k2 = internal[k1].at(0);      // pick one of its neighbors
  placements[k2] = complex<double>(radii[k1] + radii[k2]);  // place it on the real axis
  place(placements, radii, internal, k1);  // recursively place the rest
  place(placements, radii, internal, k2);

  map<int, pair<complex<double>, double> > out;
  for (map<int, double>::iterator it = radii.begin(); it != radii.end(); ++it) {
    int k = it->first;
    out[k] = pair<complex<double>, double>(placements[k], radii[k]);
  }
  
  return out;
}


// Finds a circle packing for the given configuration of internal and 
// external circles. This is the interface function for R.
//
// The two arguments are maps with disjoint keys (integer circle IDs).
// For the internal map, keys are internal circle IDs and values are vectors
// of neighbouring circle IDs. For the external map, keys are external circle
// IDs and values are radii.
//
// Returns a List (attributed as a data.frame for R) with columns for circle ID,
// centre X, centre Y and radius.
//
// [[Rcpp::export]]
List doCirclePack(List internalList, DataFrame externalDF) {
  
  // internalList is a list of vectors where, in each vector,
  // the first element is circle ID and the remaining elements are
  // IDs of the neighbouring circles.
  map<int, vector<int> > internal;
  for (int i = 0; i < internalList.size(); i++) {
    IntegerVector v = internalList(i);
    int id = v(0);
    
    vector<int> nbrs;
    for (int j = 1; j < v.size(); j++) nbrs.push_back(v(j));
    
    internal[id] = nbrs;
  }
  
  // externalDF is a DataFrame with cols ID (int) and radius (double)
  map<int, double> external;
  IntegerVector ids = externalDF[0];
  NumericVector radii = externalDF[1];
  
  for (int i = 0; i < ids.size(); i++) {
    external[ ids[i] ] = radii[i];
  }

  std::map<int, pair<complex<double>, double> > packing( CirclePack(internal, external) );

  int N = packing.size();
  
  int k = 0;
  IntegerVector out_ids(N);
  NumericVector out_xs(N);
  NumericVector out_ys(N);
  NumericVector out_radii(N);
  StringVector  out_rownames(N);

  for (map<int, pair<complex<double>, double> >::iterator it = packing.begin();
       it != packing.end(); ++it) {
         
    out_ids(k) = it->first;
    
    std::complex<double> c = it->second.first;
    out_xs(k) = c.real();
    out_ys(k) = c.imag();
    
    double r = it->second.second;
    out_radii(k) = r;
    
    out_rownames(k) = Rcpp::toString(k+1);
    
    k++ ;
  }
  
  List out_frame = List::create(
    _["id"] = out_ids,
    _["x"] = out_xs,
    _["y"] = out_ys,
    _["radius"] = out_radii );
    
  out_frame.attr("class") = "data.frame";
  out_frame.attr("row.names") = out_rownames;
  
  return out_frame;
}
