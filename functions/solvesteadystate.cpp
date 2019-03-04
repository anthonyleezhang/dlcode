
// [[Rcpp::depends(RcppArmadillo)]]

#include <math.h>
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
mat solvesteadystate(vec Fgam, vec wtp, vec best_p, mat transmat, int efficient) {
	
	int n = Fgam.n_elem;
	
	mat tradeprobmat(n,n);
	mat buyerprobmat(n,n);
	
	vec saleprob_by_seller(n);
	vec transprob(n);
	
	vec onezero = zeros(1);
	vec fgam = diff(join_cols(onezero, Fgam));
	
	//uvec transindices;
	
	double saleprob;
	
	for(int i = 0; i < n; i++) {
		transprob = fgam;
		
		// Market case, will write efficient case later
		if(efficient == 0) transprob.elem(find(wtp <= best_p(i))).zeros();
		if(efficient == 1) transprob.elem(find(Fgam <= Fgam(i))).zeros();
		
		buyerprobmat.col(i) = transprob;
		
		saleprob = sum(transprob);
		saleprob_by_seller(i) = saleprob;
		
		transprob.elem(find(Fgam == Fgam(i))) += (1-saleprob);
		tradeprobmat.col(i) = transprob;
	}
	
	mat out_matrix = transmat * tradeprobmat;
	mat ident = eye(n,n);
	mat onesmat = ones(n,n);
	vec target = ones(n,1);
	vec ss = solve(out_matrix - ident + onesmat, target);
	vec val_ss = tradeprobmat * ss;
	
	vec temp = buyerprobmat * ss;
	vec buyerdist = temp / sum(temp);
	
	temp = saleprob_by_seller % ss;
	vec seller_dist = temp / sum(temp);
	
	mat outmat(n,4);
	outmat.col(0) = ss;
	outmat.col(1) = val_ss;
	outmat.col(2) = buyerdist;
	outmat.col(3) = seller_dist;
	
	return(outmat);
}