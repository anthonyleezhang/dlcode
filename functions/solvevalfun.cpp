
// [[Rcpp::depends(RcppArmadillo)]]

#include <math.h>
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat solvevalfun(arma::vec gam, arma::vec Fgam, arma::mat transmat, 
						double tau, double delta, double Vtol, int quiet) {
	
	int n = gam.n_elem;
	
	arma::vec V = gam / (1 - delta * (1-tau));
	// Rprintf("STARTV: %f\n", startV(5));

	// initializing loop vectors
	arma::vec oldV(n);
	arma::vec EV(n);

	double sellerchunk;
	arma::vec objvec(n);
	arma::vec objvec_part(n);

	double tol = 100;
	
	while(tol > Vtol) {
		oldV = V;
		EV = transmat.t() * V;
		objvec_part = ((1 - Fgam) - tau) % (gam + delta * EV);
		for(int i = 0; i < n; i++) {
			sellerchunk = gam(i) + delta * EV(i);
			objvec = objvec_part + Fgam * sellerchunk;
			V(i) = objvec.max();
		}
		tol = max(abs(V - oldV));
		if(quiet != 1) Rprintf("TOL: %f\n", tol);
	}
	
	EV = transmat.t() * V;
	objvec_part = ((1 - Fgam) - tau) % (gam + delta * EV);
	arma::vec wtp = gam + delta * EV;
	arma::vec best_saleprob(n);
	arma::vec best_p(n);
	int myindex;
	
	for(int i = 0; i < n; i++) {
		sellerchunk = gam(i) + delta * EV(i);
		objvec = objvec_part + Fgam * sellerchunk;
		myindex = objvec.index_max();
		
		best_saleprob(i) = 1-Fgam(myindex);
		best_p(i) = wtp(myindex);
	}
	
	arma::mat outmat(n,5);
	outmat.col(0) = V;
	outmat.col(1) = EV;
	outmat.col(2) = wtp;
	outmat.col(3) = best_saleprob;
	outmat.col(4) = best_p;
	
	return(outmat);
}