// calculate conditional genotype probabilities given multipoint marker data
// [[Rcpp::export(".calc_genoprob")]]
NumericVector calc_genoprob(const IntegerMatrix& countsA, // columns are individuals, rows are markers
                            const IntegerMatrix& countsB,
                            const NumericVector& rec_frac,   // length nrow(countsA)-1
                            const double error_prob1,
                            const double error_prob2)
{
    const int n_ind = genotypes.cols();
    const int n_mar = genotypes.rows();
    const int n_gen = 2;

    // check inputs
    if(countsB.cols() != n_ind || countsB.rows() != n_mar)
        throw std::range_error("dim(countsA) != dim(countsB)");
    if(rec_frac.size() != n_mar - 1)
        throw std::range_error("length(rec_frac) != n_mar - 1");
    if(error_prob1 < 0.0 || error_prob1 > 1.0)
        throw std::range_error("error_prob1 out of range");
    if(error_prob2 < 0.0 || error_prob2 > 1.0)
        throw std::range_error("error_prob2 out of range");

    for(int i=0; i<rec_frac.size(); i++) {
        if(rec_frac[i] < 0.0 || rec_frac[i] > 0.5)
            throw std::range_error("rec_frac must be >= 0 and <= 0.5");
    }

    NumericVector genoprobs(n_mar*n_ind);

    for(int ind=0; ind<n_ind; ind++) {

        Rcpp::checkUserInterrupt();  // check for ^C from user

        // possible genotypes for this individual
        // forward/backward equations
        NumericMatrix alpha = forwardEquations(cross, genotypes(_,ind), founder_geno, is_X_chr, is_female[ind],
                                               cross_info(_,ind), rec_frac, marker_index, error_prob,
                                               poss_gen);
        NumericMatrix beta = backwardEquations(cross, genotypes(_,ind), founder_geno, is_X_chr, is_female[ind],
                                               cross_info(_,ind), rec_frac, marker_index, error_prob,
                                               poss_gen);

        // calculate genotype probabilities
        for(int pos=0, matindex=n_gen*ind; pos<n_pos; pos++, matindex += matsize) {
            int g = poss_gen[0]-1;
            double sum_at_pos = genoprobs[matindex+g] = alpha(0,pos) + beta(0,pos);
            for(int i=1; i<n_poss_gen; i++) {
                int g = poss_gen[i]-1;
                double val = genoprobs[matindex+g] = alpha(i,pos) + beta(i,pos);
                sum_at_pos = addlog(sum_at_pos, val);
            }
            for(int i=0; i<n_poss_gen; i++) {
                int g = poss_gen[i]-1;
                genoprobs[matindex+g] = exp(genoprobs[matindex+g] - sum_at_pos);
            }
        }
    } // loop over individuals

    genoprobs.attr("dim") = Dimension(n_gen, n_ind, n_mar);
    return genoprobs;
}



// forward equations
NumericMatrix forwardEquations(const IntegerVector& countsA,
                               const IntegerVector& countsB,
                               const NumericVector& rec_frac,
                               const double error_prob1,
                               const double error_prob2)
{
    const int n_mar = countsA.size();
    const int n_gen = 2;

    // to contain ln Pr(G_i = g | marker data)
    NumericMatrix alpha(n_gen, n_mar);

    // initialize alphas
    for(int i=0; i<n_gen; i++) {
        alpha(i,0) = init(i) +
            emit(countsA[0], countsB[0], i, error_prob1, error_prob2);
    }

    for(int mar=1; mar<n_mar; mar++) {
        for(int ir=0; ir<n_gen; ir++) {
            alpha(ir,mar) = alpha(0, mar-1) + step(0, ir, rec_frac[mar-1]);

            for(int il=1; il<n_gen; il++)
                alpha(ir,mar) = addlog(alpha(ir,mar), alpha(il,mar-1) +
                                       step(il, ir, rec_frac[mar-1]));

            alpha(ir,mar) += emit(countsA[mar], countsB[mar], ir, error_prob1, error_prob2);
        }
    }

    return alpha;
}



// backward Equations
NumericMatrix backwardEquations(const IntegerVector& countsA,
                                const IntegerVector& countsB,
                                const NumericVector& rec_frac,
                                const double error_prob1,
                                const double error_prob2)
{
    const int n_mar = countsA.size();
    const int n_gen = 2;

    // to contain ln Pr(G_i = g | marker data)
    NumericMatrix beta(n_gen, n_mar)

    // backward equations
    for(int mar = n_mar-2; mar >= 0; mar--) {
        for(int il=0; il<n_gen; il++) {
            for(int ir=0; ir<n_gen; ir++) {
                double to_add = beta(ir,mar+1) + step(il, ir, rec_frac[mar]) +
                    emit(countsA[mar], countsB[mar], ir, error_prob1, error_prob2);

                if(ir==0) beta(il,mar) = to_add;
                else beta(il,mar) = addlog(beta(il,mar), to_add);
            }
        }
    }

    return beta;
}


// Calculate addlog(a,b) = log[exp(a) + exp(b)]
// [[Rcpp::export]]
double addlog(const double a, const double b)
{
    const double tol=200.0;

    // if both -Inf, return -Inf
    if(Rcpp::traits::is_infinite<REALSXP>(a) &&
       Rcpp::traits::is_infinite<REALSXP>(b) && a < 0 && b < 0) return a;

    if(b > a + tol) return b;
    else if(a > b + tol) return a;
    else return a + log1p(exp(b-a));
}


// init probability for backcross
const double init(const int true_gen) // assume true_gen = 0 or 1
{
    return log(0.5);
}

// step probability for backcross (1-rf or rf)
const double step(const int gen_left, const int gen_right, const double rec_frac)
{
    if(gen_left==gen_right) return log(1.0-rec_frac);
    else return log(rec_frac);
}

// emit probability for backcross; GBS data
const double emit(const int countA, const int countB, const int true_gen,
                  const double error_prob1, const double error_prob2)
{
    double prob_het = (countA+countB)*log(0.5);
    double prob_hom = countA*log(1-error_prob1) + countB*log(error_prob1);

    if(true_gen == 0) // homozygous
        return(log( (1.0-error_prob2)*exp(prob_hom) + error_prob2*exp(prob_het)));
    else // heterozygous
        return(log( (1.0-error_prob2)*exp(prob_het) + error_prob2*exp(prob_hom)));
}
