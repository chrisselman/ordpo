// Simple logistic regression model for Bernoulli outcome

data {
  int N;
  int K;
  matrix[N, K] X;
  array[N] int y;
  vector[K] beta_sd;
}

parameters {
  vector[K] beta_raw;
}

transformed parameters {
  vector[K] beta = beta_sd .* beta_raw;
}

model {
  // lp
  vector[N] eta = X * beta;
  // prior
  target += std_normal_lpdf(beta_raw);
  // likelihood
  target += bernoulli_logit_lpmf(y | eta);
}
