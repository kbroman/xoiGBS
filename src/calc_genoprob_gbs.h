// calculate conditional genotype probabilities given multipoint marker data
#ifndef CALC_GENOPROB_GBS_H
#define CALC_GENOPROB_GBS_H

// calculate conditional genotype probabilities given multipoint marker data
// [[Rcpp::export(".calc_genoprob")]]
NumericVector calc_genoprob_gbs(const IntegerMatrix& countsA, // columns are individuals, rows are markers
                            const IntegerMatrix& countsB,
                            const NumericVector& rec_frac,   // length nrow(countsA)-1
                            const double error_prob1,
                            const double error_prob2);


// forward equations
NumericMatrix forwardEquations(const IntegerVector& countsA,
                               const IntegerVector& countsB,
                               const NumericVector& rec_frac,
                               const double error_prob1,
                               const double error_prob2);


// backward Equations
NumericMatrix backwardEquations(const IntegerVector& countsA,
                                const IntegerVector& countsB,
                                const NumericVector& rec_frac,
                                const double error_prob1,
                                const double error_prob2);

// Calculate addlog(a,b) = log[exp(a) + exp(b)]
// [[Rcpp::export]]
double addlog(const double a, const double b);

// init probability for backcross
double init(const int true_gen); // assume true_gen = 0 or 1

// step probability for backcross (1-rf or rf)
double step(const int gen_left, const int gen_right, const double rec_frac);

// emit probability for backcross; GBS data
double emit(const int countA, const int countB, const int true_gen,
            const double error_prob1, const double error_prob2);


#endif // CALC_GENOPROB_GBS_H
