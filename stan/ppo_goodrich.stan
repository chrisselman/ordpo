// Based on lrmqr.stan by Ben Goodrich
functions {
  // pointwise log-likelihood contributions
  vector pw_log_lik(vector alpha, vector beta, matrix tau, 
	                  row_vector[] X, row_vector[] Z, int[] y) {
    int N = size(X);
    vector[N] out;
    int k = max(y); // assumes all possible categories are observed
    for (n in 1:N) {
      real eta = X[n] * beta;
      int j = y[n];
  		real cj;
  		real cj1;
			if (j == 1)        cj  = -( alpha[1] + eta );
			else if (j == 2)   cj  = alpha[1] + eta;
			else               cj  = alpha[j - 1] + eta + Z[n] * tau[ , j - 2];
			if(j > 1 && j < k) cj1 = alpha[j] + eta + Z[n] * tau[ , j - 1];

      if (j == 1 || j == k) out[n] = log_inv_logit(cj);
			else out[n] = log(1./(1. + exp(-cj)) - 1./(1. + exp(-cj1)));
//      else out[n] = log_diff_exp(-log1p_exp(- cj),
//                                 -log1p_exp(- cj1));
//			else out[n] = log(-log1p_exp(-cj) + log1p_exp(-cj1));
    }
    return out;
  }
  
  // Pr(y == j)
  matrix Pr(vector alpha, vector beta, matrix tau,
	          row_vector[] X, row_vector[] Z, int[] y) {
    int N = size(X);
    int k = max(y); // assumes all possible categories are observed
    matrix[N, k] out;

		for(n in 1:N) {
      real eta = X[n] * beta;
			for(j in 1 : k) {
			  real cj;
		    real cj1;
			  if (j == 1)        cj  = -( alpha[1] + eta );
			  else if (j == 2)   cj  = alpha[1] + eta;
			  else               cj  = alpha[j - 1] + eta + Z[n] * tau[ , j - 2];
			  if(j > 1 && j < k) cj1 = alpha[j] + eta + Z[n] * tau[ , j - 1];

				if (j == 1 || j == k) out[n, j] = log_inv_logit(cj);
				else out[n, j] = log(1./(1. + exp(-cj)) - 1./(1. + exp(-cj1)));
//				else  out[n, j] = log_diff_exp(-log1p_exp(-cj),
//                                   -log1p_exp(-cj1));
//				else out[n, j] = log(-log1p_exp(-cj) + log1p_exp(-cj1));
			}
    }
    return exp(out);
  }
}
data {
  int<lower = 1> N;   // number of observations
  int<lower = 1> p;   // number of predictors
	int<lower = 1> q;   // number of non-PO predictors in Z
  matrix[N, p] X;     // matrix of CENTERED predictors
	matrix[N, q] Z;     // matrix of CENTERED PPO predictors
  int<lower = 2> k;   // number of outcome categories
  int<lower = 1, upper = k> y[N]; // outcome on 1 ... k
  
  // prior standard deviations
  real<lower=0> sds;
	real<lower=0> sdsppo;

  real<lower = 0> conc;
}

transformed data {
	row_vector[p] Xr[N];
	row_vector[q] Zr[N];
  for (n in 1:N) Xr[n] = X[n, ];
	for (n in 1:N) Zr[n] = Z[n, ];
}

parameters {
  vector[p] beta; // coefficients on X
  matrix[q, k - 2] tau;  // coefficients on Z
  simplex[k] pi;  // category probabilities for a person w/ average predictors
}

transformed parameters {
  vector[k - 1] alpha;                               // intercepts
  vector[N] log_lik;                                 // log-likelihood pieces
  for (j in 2:k) alpha[j - 1] = logit(sum(pi[j:k])); // predictors are CENTERED
  log_lik = pw_log_lik(alpha, beta, tau, Xr, Zr, y);
}

model {
  target += log_lik;
  target += normal_lpdf(beta | 0, sds);
  target += dirichlet_lpdf(pi | rep_vector(conc, k));
	for (j in 1:(k - 2)) target += normal_lpdf(tau[ , j] | 0, sdsppo);
  // implicit: pi ~ dirichlet(ones)
}