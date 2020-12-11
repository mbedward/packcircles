/*
 * Progressive circle packing algorithm. 
 * 
 * This is a port of C code written by Peter Menzel (https://github.com/pmenzel/packCircles)
 * and ported to R/Rcpp with his permission.
 * 
 * ========== Original header (minus compilation and usage notes) ==========
 * 
 *  packCircles 1.0
 *  
 *  author: Peter Menzel
 *
 *  ABOUT
 *  ********
 *	packCircles arranges a list of circles, which are denoted by their radii,
 *	by consecutively placing each circle externally tangent to two previously placed
 *	circles avoiding overlaps.
 *
 *	The program implements the algorithm described in the paper:
 *  "Visualization of large hierarchical data by circle packing" 
 *  by Weixin Wang, Hui Wang, Guozhong Dai, and Hongan Wang
 *  in Proceedings of the SIGCHI Conference on Human Factors in Computing Systems, 2006, pp. 517-520
 *  https://doi.org/10.1145/1124772.1124851
 *
 *  Source code is partially based on a implementation of this algorithm
 *  in the ProtoVis javascript library:
 *  https://mbostock.github.io/protovis/
 *
 *
 * ========== Original license (BSD 2-clause simplified) ==========
 * 
 * Copyright (c) 2016, Peter Menzel
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * 
 * - Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *  
 */

#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <float.h>
using namespace Rcpp;

const double INTERSECTION_TOL = 1.0e-4;


class Node {
public:
  Node() { init(); }
  
  Node(double r_) {
    init();
    radius = r_;
  }
  
  void init() {
    x = 0.0;
    y = 0.0;
    radius = 0.0;
    next = NULL;
    prev = NULL;
    insertnext = NULL;
  }
  
  // Check for intersection with another node
  bool intersects(Node* n) {
    double dx = x - n->x;
    double dy = y - n->y;
    double dr = radius + n->radius;
    
    return ((dr * dr - dx * dx - dy * dy) > INTERSECTION_TOL);
  }
  
  // Place this node after node `a`
  void place_after(Node *a) {
    Node *n = a->next;
    a->next = this;
    this->prev = a;
    this->next = n;
    if (n) n->prev = this;
  }
  
  // Splice this node before node `a`
  void splice(Node *a) {
    this->next = a;
    a->prev = this;
  }

  double radius;
  double x;
  double y;
  
  Node* next;
  Node* prev;
  Node* insertnext;
};


void place_circle(Node* a, Node* b, Node* c) {
  double da = b->radius + c->radius;
  double db = a->radius + c->radius;
  double dx = b->x - a->x;
  double dy = b->y - a->y;
  double dc = sqrt(dx * dx + dy * dy);
  if (dc > 0.0) {
    double cos = (db * db + dc * dc - da * da) / (2 * db * dc);
    double theta = acos(cos);
    double x = cos * db;
    double h = sin(theta) * db;
    dx /= dc;
    dy /= dc;
    
    c->x = a->x + x * dx + h * dy;
    c->y = a->y + x * dy - h * dx;
  }
  else {
    c->x = a->x + db;
    c->y = a->y;
  }
}


void place_circles(Node* firstnode) {
  Node * a = firstnode;
  Node * b = NULL;
  Node * c = NULL;
  
  // First circle
  a->x = -1 * a->radius;

  // Second circle
  if(!a->insertnext) return;
  b = a->insertnext;
  b->x = b->radius;
  b->y = 0;

  // Third circle
  if(!b->insertnext) return;
  c = b->insertnext;
  place_circle(a, b, c);  
  if (!c->insertnext) return;
    
  // Initial node chain
  // -> a <--> c <--> b <-
  a->next = c;
  a->prev = b;
  b->next = a;
  b->prev = c;
  c->next = b;
  c->prev = a;
  b = c;
  
  c = c->insertnext;
  bool skip = false;
  
  while(c) {
    // pmenzel's comment:
    // Determine the node a in the chain, which is nearest to the center
    // The new node c will be placed next to a (unless overlap occurs)
    // NB: This search is only done the first time for each new node, i.e.
    // not again after splicing.
    if(!skip) {
      Node* n = a;
      Node* nearestnode = n;
      double nearestdist = FLT_MAX;
      
      do {
        double dist_n = sqrt(n->x * n->x + n->y * n->y);
        if(dist_n < nearestdist) {	
          nearestdist = dist_n;
          nearestnode = n;
        }
        n = n->next;
      } while(n != a);
      
      a = nearestnode; 
      b = nearestnode->next;
      skip=false;
    }
    
    place_circle(a, b, c);
    
    // Search for possible closest intersection
    bool isect = false;
    Node* j = b->next;
    Node* k = a->prev;
    
    double sj = b->radius;
    double sk = a->radius;
    
    do {
      if (sj <= sk) {
        if ( j->intersects(c) ) {
          a->splice(j);
          b = j;
          skip = true;
          isect = true;
          break;
        }
        sj += j->radius;
        j = j->next;
      }
      else {
        if( c->intersects(k) ) {
          k->splice(b);
          a = k;
          skip = true;
          isect = true;
          break;
        }
        sk += k->radius;
        k = k->prev;
      }
    } while (j != k->next);
    
    // Update the node chain
    if(!isect) {
      c->place_after(a);
      b = c;
      
      skip = false;
      
      c = c->insertnext;
    }
  }
}


// [[Rcpp::export]]
DataFrame do_progressive_layout(NumericVector radii) {
  int N = radii.length();
  
  Node *node = new Node(radii(0));
  Node *firstnode = node;
  Node *lastinsertednode = node;

  for (int i = 1; i < N; i++) {
    node = new Node(radii[i]);
    lastinsertednode->insertnext = node;
    
    lastinsertednode = node;
  }
  
  place_circles(firstnode);
  
  NumericVector xs(N);
  NumericVector ys(N);
  
  // Retrieve circle positions and free memory
  node = firstnode;
  int i = 0;
  while (node) {
    xs[i] = node->x;
    ys[i] = node->y;
    radii[i] = node->radius;
    node = node->insertnext;
    i++;
  }

  node = firstnode;
  while (node) {
    Node *next = node->insertnext;
    delete node;
    node = next;
  }

  return DataFrame::create(
    Named("x") = xs,
    Named("y") = ys,
    Named("radius") = radii);
}

