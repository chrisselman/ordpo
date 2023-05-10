functions {
  // pointwise log-likelihood contributions
  vector pw_log_lik(vector alpha, vector beta, vector tau, vector pposcore, 
          row_vector[] X, row_vector[] Z, int[] y) 
	                  {
    int N = size(X);
    vector[N] out;
    int k = max(y); // assumes all possible categories are observed
		real zeta = 0.;
		real r = 0.;
		real r2 = 0.;
		real ca;
  	real ca1;
		int a;
		int q = size(Z) > 0 ? cols(Z[1]) : 0;
    for (n in 1:N) {
      real eta  = X[n] * beta;
			if(q > 0)  zeta = Z[n] * tau;
      a = y[n];    
			  if(q == 0) {  // PO
  			  if (a == 1)        ca  = -(alpha[1]     + eta);
	  		  else if (a == 2)   ca  =   alpha[1]     + eta;
		  	  else               ca  =   alpha[a - 1] + eta;
			    if(a > 1 && a < k) ca1 =   alpha[a]     + eta;
				}
				else {
  			  if (a == 1)        ca  = -( alpha[1]     + eta + pposcore[2]   * zeta);
	  		  else if (a == 2)   ca  =    alpha[1]     + eta + pposcore[2]   * zeta;
		  	  else               ca  =    alpha[a - 1] + eta + pposcore[a]   * zeta;
			    if(a > 1 && a < k) ca1 =    alpha[a]     + eta + pposcore[a+1] * zeta;
				}
      if (a == 1 || a == k) out[n] = log_inv_logit(ca);
			else out[n]  = log(1./(1. + exp(-ca)) - 1./(1. + exp(-ca1)));
			 // if(q > 0) r = pposcore[a] * zeta;
			//	out[n] = log_inv_logit(alpha[a-1] + eta + r);
    }
    return out;
  }
}

data {
  int<lower = 1> N;   // number of observations
  int<lower = 1> p;   // number of predictors
	int<lower = 0> q;   // number of non-PO predictors in Z
  int<lower = 2> k;   // number of outcome categories
	int<lower = 0, upper = k> lpposcore;  // extent of pposcore (1=PO)
  matrix[N, p] X;     // matrix of CENTERED predictors
	matrix[N, q] Z;     // matrix of CENTERED PPO predictors
  int<lower = 1, upper = k> y[N]; // outcome on 1 ... k
	vector[lpposcore] pposcore; // scores for constrained partial PO

// prior standard deviations
  real<lower = 0> sds;
  real<lower = 0> sdsppo;
}

transformed data {
	row_vector[p] Xr[N];
	row_vector[q] Zr[N];
  for (n in 1:N) Xr[n] = X[n, ];
	for (n in 1:N) Zr[n] = Z[n, ];
}

parameters {
  vector[p] beta; // coefficients on X
  vector[q] tau;  // coefficients on Z
  simplex[k] pi;  // category probabilities for a person w/ average predictors
}

transformed parameters {
  vector[k - 1] alpha;                               // intercepts
  vector[N] log_lik;                                 // log-likelihood pieces
  for (j in 2:k) alpha[j - 1] = logit(sum(pi[j:k])); // predictors are CENTERED
  log_lik = pw_log_lik(alpha, beta, tau, pposcore, Xr, Zr, y);
}

model {
  target += log_lik;
  target += normal_lpdf(beta | 0, sds);
	if(q > 0) target += normal_lpdf(tau | 0, sdsppo);
}

generated quantities {
  vector[p] OR1 = exp(beta + pposcore[2]*tau[1]);
  vector[p] OR2 = exp(beta + pposcore[3]*tau[1]);
}