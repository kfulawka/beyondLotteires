functions {
  
  // probability weighting function
  real pwf(real p, real gam, real del) {
    
    real wp;
    
    wp = (del*p^gam) / ( del*p^gam + (1-p)^gam ); 
    
    return wp;
  }
  
  real gam_af(real g, real a) {
    
    real g_af = g^abs(a);
    
    return g_af;
    
  }
  
  // value function
  real vf(real x, real alp) {
    
    real v;
    
    v = -1 * fabs(x)^alp ;
    
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
  vector[N] gam_z;
  vector[N] theta_z;
  
  // spt means on probit scale
  real gam_mu_phi;
  real theta_mu_phi;
  
  // spt scales on probit
  real<lower = 0> sig_gam;
  real<lower = 0> sig_theta;

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
  
  matrix[n, 2] gam_aw;
  matrix[n, 2] max_o;
  
  // id-lvl parameters for modeling
  vector[N] gam = Phi(gam_mu_phi + sig_gam * gam_z);
  vector[N] theta = Phi(theta_mu_phi + sig_theta * theta_z) * 5;


  // for each data point with SOME search
  for(i in 1:n) {
    
    // AFFECT-POOR //////////////////////////////////////////////
    
    max_o[i,1] = max([fabs(xa[i,1]), fabs(xb[i,1])]) / 10;
    
    gam_aw[i,1] = gam_af(gam[sub[i]], max_o[i,1]);
      
    // A
    v_xa[i,1] = vf(xa[i,1], 1);

    w_pa[i,1] = pwf(pa[i,1], gam_aw[i,1], 1);

    sv_p[i,1] = v_xa[i,1] * w_pa[i,1] ;
    
    // B
    v_xb[i,1] = vf(xb[i,1], 1);
    
    w_pb[i,1] = pwf(pb[i,1], gam_aw[i,1], 1);

    sv_p[i,2] = v_xb[i,1] * w_pb[i,1] ;
    
    // difference in favor of A
    ap_sv_diff[i] = theta[ sub[i] ] * (sv_p[i,1] - sv_p[i,2]);
    
    // AFFECT-RICH //////////////////////////////////////////////
    
    max_o[i,2] = max([fabs(xa[i,2]), fabs(xb[i,2])]) / 10;
    
    gam_aw[i,2] = gam_af(gam[sub[i]], max_o[i,2]);

    // A
    v_xa[i,2] = vf(xa[i,2], 1);
    
    w_pa[i,2] = pwf(pa[i,2], gam_aw[i,2], 1);
    
    sv_r[i,1] = v_xa[i,2] * w_pa[i,2] ;
    
    // B
    v_xb[i,2] = vf(xb[i,2], 1);

    w_pb[i,2] = pwf(pb[i,2], gam_aw[i,2], 1);

    sv_r[i,2] = v_xb[i,2] * w_pb[i,2] ;
    
    // difference in favor of A
    ar_sv_diff[i] = theta[ sub[i] ] * (sv_r[i,1] - sv_r[i,2]);

  }
  
}

model {
  
  // spt pop-level parameters
  gam_mu_phi ~ std_normal();
  to_vector(gam_z) ~ std_normal();
  sig_gam ~ normal(.5, .13);

  theta_mu_phi ~ std_normal();
  to_vector(theta_z) ~ std_normal();
  sig_theta ~ normal(.5, .13);
  
  // likelihood
  co[,1] ~ bernoulli_logit( ap_sv_diff ); // affect poor
  co[,2] ~ bernoulli_logit( ar_sv_diff ); // affect rich
  
}

// logliks for loo
generated quantities {

  real log_lik_ap[n];
  real log_lik_ar[n];

  // transform the means
  real mu_gam = Phi(gam_mu_phi);
  real mu_theta = Phi(theta_mu_phi) * 5;

  // log liks for elpd_loo
  for(i in 1:n) {
    
    log_lik_ap[i] = bernoulli_logit_lpmf( co[i,1] | ap_sv_diff[i] );
    log_lik_ar[i] = bernoulli_logit_lpmf( co[i,2] | ar_sv_diff[i] );
    
  }
  
}