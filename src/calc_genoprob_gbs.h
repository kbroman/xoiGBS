// calculate conditional genotype probabilities given multipoint marker data
#ifndef CALC_GENOPROB_GBS_H
#define CALC_GENOPROB_GBS_H

#include <Rcpp.h>

using namespace Rcpp;

// calculate conditional genotype probabilities given multipoint marker data
Rcpp::NumericVector calc_genoprob_gbs(const Rcpp::IntegerMatrix& countsA, // columns are individuals, rows are markers
                                      const Rcpp::IntegerMatrix& countsB,
                                      const Rcpp::NumericVector& rec_frac,   // length nrow(countsA)-1
                                      const double error_prob1,
                                      const double error_prob2);


// forward equations
Rcpp::NumericMatrix forwardEquations(const Rcpp::IntegerVector& countsA,
                                     const Rcpp::IntegerVector& countsB,
                                     const Rcpp::NumericVector& rec_frac,
                                     const double error_prob1,
                                     const double error_prob2);


// backward Equations
Rcpp::NumericMatrix backwardEquations(const Rcpp::IntegerVector& countsA,
                                      const Rcpp::IntegerVector& countsB,
                                      const Rcpp::NumericVector& rec_frac,
                                      const double error_prob1,
                                      const double error_prob2);

// Calculate addlog(a,b) = log[exp(a) + exp(b)]
double addlog(const double a, const double b);

// init probability for backcross
double init(const int true_gen); // assume true_gen = 0 or 1

// step probability for backcross (1-rf or rf)
double step(const int gen_left, const int gen_right, const double rec_frac);

// emit probability for backcross; GBS data
double emit(const int countA, const int countB, const int true_gen,
            const double error_prob1, const double error_prob2);


#endif // CALC_GENOPROB_GBS_H
