functions {
  
  // probability weighting functino
  real pwf(real p, real gam, real del) {
    
    real wp;
    
    wp = (del*p^gam) / ( del*p^gam + (1-p)^gam ); 
    
    return wp;
  }
  
  // value function
  real vf(real x) {
    
    real v;
    
    v = -1 * fabs(x) ;
    
    return v;
  }
}

data {
  
  // n - no. of data points per condition
  int<lower = 0> n;
  
  // N - no. of subjects
  int<lower = 0> N;
  
  // subject ind vector
  int<lower = 1> sub[n];
  
  // drug A objective pars
  matrix[n, 2] xa;
  matrix[n, 2] pa;
  
  // drug B objective pars
  matrix[n, 2] xb;
  matrix[n, 2] pb;
  
  // choices: col1 - affect poor; col2 - affect rich
  int<lower = 0, upper = 1> co[n, 2];
  
}

parameters {
  
  // spt individual parameters on z scale
  matrix[2, N] gam_z;
  matrix[2, N] theta_z;
  
  // spt means on probit scale
  row_vector[2] gam_mu_phi;
  row_vector[2] theta_mu_phi;
  
  // spt scales on probit
  vector<lower = 0>[2] gam_sigma;
  vector<lower = 0>[2] theta_sigma;
  
  // spt pars corr matrix
  cholesky_factor_corr[2] L_gam_omega;
  cholesky_factor_corr[2] L_theta_omega;
  
}

transformed parameters {
  
  // spt sv cpmutpation
  matrix[n, 2] v_xa;
  matrix[n, 2] w_pa;
  
  matrix[n, 2] v_xb;
  matrix[n, 2] w_pb;
  
  matrix[n, 2] sv_p;
  matrix[n, 2] sv_r;
  
  vector[n] ap_sv_diff;
  vector[n] ar_sv_diff;
  
  // parameters on wanted scale
  vector[N] del_ap;
  vector[N] del_ar;
  vector[N] gam_ap;
  vector[N] gam_ar;
  vector[N] theta_ap;
  vector[N] theta_ar;
  
  // individual level pars on probit scale
  matrix[N, 2] gam_phi;
  matrix[N, 2] theta_phi;
  
  // transform individual parameters
  // this makes them dependent on the multivariate normal dist
  gam_phi = (diag_pre_multiply(gam_sigma, L_gam_omega) * gam_z)';
  theta_phi = (diag_pre_multiply(theta_sigma, L_theta_omega) * theta_z)';

  gam_ap = Phi_approx( gam_mu_phi[1] + gam_phi[,1] );
  gam_ar = Phi_approx( gam_mu_phi[2] + gam_phi[,2] );
  theta_ap = exp( theta_mu_phi[1] + theta_phi[,1] );
  theta_ar = exp( theta_mu_phi[2] + theta_phi[,2] );
  
  // for each data point with SOME search
  for(i in 1:n) {
    
    // AFFECT-POOR //////////////////////////////////////////////
      
    // A
    v_xa[i,1] = vf(xa[i,1]);
    w_pa[i,1] = pwf(pa[i,1], gam_ap[ sub[i] ], 1);

    sv_p[i,1] = v_xa[i,1] * w_pa[i,1] ;
    
    // B
    v_xb[i,1] = vf(xb[i,1]);
    w_pb[i,1] = pwf(pb[i,1], gam_ap[ sub[i] ], 1);

    sv_p[i,2] = v_xb[i,1] * w_pb[i,1] ;
    
    // difference in favor of A
    ap_sv_diff[i] = theta_ap[ sub[i] ] * (sv_p[i,1] - sv_p[i,2]);
    
    // AFFECT-RICH //////////////////////////////////////////////
    
    // A
    v_xa[i,2] = vf(xa[i,2]);
    w_pa[i,2] = pwf(pa[i,2], gam_ar[ sub[i] ], 1);

    sv_r[i,1] = v_xa[i,2] * w_pa[i,2] ;
    
    // B
    v_xb[i,2] = vf(xb[i,2]);
    w_pb[i,2] = pwf(pb[i,2], gam_ar[ sub[i] ], 1);

    sv_r[i,2] = v_xb[i,2] * w_pb[i,2] ;
    
    // difference in favor of A
    ar_sv_diff[i] = theta_ar[ sub[i] ] * (sv_r[i,1] - sv_r[i,2]);

  }
  
}

model {

  gam_mu_phi ~ std_normal();
  to_vector(gam_z) ~ std_normal();
  gam_sigma ~ normal(.5, .13);
  L_gam_omega ~ lkj_corr_cholesky(3);
  
  theta_mu_phi ~ std_normal();
  to_vector(theta_z) ~ std_normal();
  theta_sigma ~ normal(.5, .13);
  
  // likelihood
  co[,1] ~ bernoulli_logit( ap_sv_diff ); // affect poor
  co[,2] ~ bernoulli_logit( ar_sv_diff ); // affect rich
  
}

// logliks for loo
generated quantities {
  
  matrix[2,2] gam_omega;
  matrix[2,2] theta_omega;
  real gam_r;
  real theta_r;
  
  real log_lik_ap[n];
  real log_lik_ar[n];

  // transform the means
  real mu_gam_ap = Phi_approx(gam_mu_phi[1]);
  real mu_gam_ar = Phi_approx(gam_mu_phi[2]);
  real mu_theta_ap = exp(theta_mu_phi[1]);
  real mu_theta_ar = exp(theta_mu_phi[2]);

  // get the sigmas
  real sig_gam_ap = gam_sigma[1];
  real sig_gam_ar = gam_sigma[2];
  real sig_theta_ap = theta_sigma[1];
  real sig_theta_ar = theta_sigma[2];

  // cor matrix
  gam_omega = L_gam_omega * L_gam_omega';
  gam_r = gam_omega[1,2];
  
  theta_omega = L_theta_omega * L_theta_omega';
  theta_r = theta_omega[1,2];
  
  
  // log liks for elpd_loo
  for(i in 1:n) {
    
    log_lik_ap[i] = bernoulli_logit_lpmf( co[i,1] | ap_sv_diff[i] );
    log_lik_ar[i] = bernoulli_logit_lpmf( co[i,2] | ar_sv_diff[i] );
    
  }
  
}