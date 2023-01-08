data{
  int T;
  int T_forward;
  int T_backward;
  int P;
  int n; 
  int HOS_obs[n];
  int NOS_obs[n];
  int pop_obs[n];
  int year_obs[n];
}
transformed data{
  int TOS_obs[n];
  for(i in 1:n){
    TOS_obs[i] = HOS_obs[i] + NOS_obs[i];
  }
}
parameters{
  matrix[T-1,P] eps;
  vector<lower=0,upper=1>[P] p_0;
  real<lower=0> sigma_rn_mu;
  real<lower=0> sigma_rn_sigma;
  vector[P] eps_sigma_rn; 
  cholesky_factor_corr[P] L;
}
transformed parameters{
  matrix<lower=0,upper=1>[T,P] p;
  vector<lower=0>[P] sigma_rn = exp(log(sigma_rn_mu) + eps_sigma_rn * sigma_rn_sigma); 
  p[1,1:P] = to_row_vector(p_0[1:P]);
  for(t in 2:T){
    p[t,1:P] = to_row_vector(inv_logit(to_vector(logit(p[t-1,1:P])) + diag_pre_multiply(sigma_rn,L) * to_vector(eps[t-1,1:P])));
  }
}
model{
  vector[n] local_p;
  for(i in 1:n){
    local_p[i] = p[year_obs[i],pop_obs[i]];
  }
  //=========Priors================
  //observation  & process error sds
  sigma_rn_mu ~ std_normal(); 
  sigma_rn_sigma ~ std_normal();
  eps_sigma_rn ~ std_normal();
  //correlation matrix
  L ~ lkj_corr_cholesky(1);
  //process errors
  to_vector(eps) ~ std_normal();
  //initial states
  p_0 ~ beta(1,1);
  //=========likelihood=============
  HOS_obs ~ binomial(TOS_obs,local_p);
}
generated quantities{
  matrix[P,P] Omega = multiply_lower_tri_self_transpose(L);
  matrix[P,P] Sigma = quad_form_diag(Omega, sigma_rn);
  matrix[T + T_forward + T_backward,P] p_all;
  p_all[T_backward + 1:T_backward + T,1:P] = p;
  matrix[T_forward,P] eps_pred_forward;
  matrix[T_backward,P] eps_pred_backward;
  for(t in (T_backward + T + 1):(T_backward + T + T_forward)){
    for(i in 1:P){
      eps_pred_forward[t,i] = normal_rng(0,1);
    }
    p_all[t,1:P] = to_row_vector(exp(to_vector(log(p_all[t-1,1:P])) + L * to_vector(eps_pred_forward[t,1:P])));
  }
  for(t in 1 : T_backward){
    for(i in 1:P){
      eps_pred_backward[t,i] = normal_rng(0,1);
    }
    p_all[T_backward - t + 1,1:P] = to_row_vector(exp(to_vector(log(p_all[T_backward - t + 2,1:P])) - L * to_vector(eps_pred_backward[t,1:P])));
  }
}
