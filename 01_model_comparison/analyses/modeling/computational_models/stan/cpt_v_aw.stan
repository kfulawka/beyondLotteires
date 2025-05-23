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
  
  real del_af(real d, real a) {

    real d_af = d * abs(a);

    return d_af;

  }
  
  // value function
  real vf(real s, real x, real alp) {
    
    real v;
    
    v = s * fabs(x)^alp ;
    
    return v;
    
  }
}

data {
  int n;
  int n_cp;
  matrix[n_cp, n] xA;
  matrix[n_cp, n] xB;
  matrix[n_cp, n] aA;
  matrix[n_cp, n] aB;
  matrix[n_cp, 2] p;
  int<lower = 0, upper = 1> co[n_cp, n];
  int s;
  int max_a;
}

parameters {
  
  real mu_alp_Phi;
  real<lower = 0> sigma_alp;
  vector[n] alp_Phi;
  
  real mu_del_Phi;
  real<lower = 0> sigma_del;
  vector[n] del_Phi;
  
  real mu_gam_Phi;
  real<lower = 0> sigma_gam;
  vector[n] gam_Phi;
  
  real mu_theta_Phi;
  real<lower = 0> sigma_theta;
  vector[n] theta_Phi;
  
}

transformed parameters {
  
  // subjective values and weights
  matrix[n_cp, n] vXa;
  matrix[n_cp, n] wXa;
  matrix[n_cp, n] vsA;
  
  matrix[n_cp, n] vXb;
  matrix[n_cp, n] wXb;
  matrix[n_cp, n] vsB;
  
  matrix[n_cp, n] del_a;
  matrix[n_cp, n] gam_a;
  
  matrix[n_cp, n] max_d_ar;
  
  // parameters on wanted scales
  vector[n] alp;
  vector[n] del;
  vector[n] gam;
  vector[n] theta;
  
  // scaling
  alp = Phi(mu_alp_Phi + sigma_alp * alp_Phi);
  gam = Phi(mu_gam_Phi + sigma_gam * gam_Phi);
  del = Phi(mu_del_Phi + sigma_del * del_Phi) * 10;
  theta = Phi(mu_theta_Phi + sigma_theta * theta_Phi) * 5;
  
  // CPT
  for (j in 1:n) {
    for (i in 1:n_cp) {

      // adw pars
      max_d_ar[i,j] = max( [fabs(aA[i,j]), fabs(aB[i,j]) ] )/max_a;
      gam_a[i,j] = gam_af(gam[j], max_d_ar[i,j]);
      del_a[i,j] = del_af(del[j], max_d_ar[i,j]);
      
      
      // option A
      vXa[i,j] = vf(s, xA[i,j], alp[j]);
      
      wXa[i,j] = pwf(p[i,1], gam_a[i,j], del_a[i,j]);
      
      vsA[i,j] = vXa[i,j] * wXa[i,j];
      
      // option B
      vXb[i,j] = vf(s, xB[i,j], alp[j]);      

      wXb[i,j] = pwf(p[i,2], gam_a[i,j], del_a[i,j]);
      
      vsB[i,j] = vXb[i,j] * wXb[i,j];
      
    }
  }
}

model {
  
  mu_alp_Phi ~ std_normal();
  sigma_alp ~ normal(.5, .13);

  mu_gam_Phi ~ std_normal();
  sigma_gam ~ normal(.5, .13);
  
  mu_del_Phi ~ std_normal();
  sigma_del ~ normal(.5, .13);
  
  mu_theta_Phi ~ std_normal();
  sigma_theta ~ normal(.5, .13);
  
  // individual parameters (vectorized)
  alp_Phi ~ std_normal();
  gam_Phi ~ std_normal();
  del_Phi ~ std_normal();
  theta_Phi ~ std_normal();
  
  // CPT
  for (j in 1:n) {
    for (i in 1:n_cp) {
      
      co[i,j] ~ bernoulli_logit( theta[j] * (vsA[i,j] - vsB[i,j]) );
      
    }
  }
}

// logliks for loo
generated quantities {

  matrix[n_cp, n] log_lik;
  matrix[n_cp, n] pa;

  real mu_alp = Phi(mu_alp_Phi);
  real mu_gam = Phi(mu_gam_Phi);
  real mu_del = Phi(mu_del_Phi) * 10;
  real mu_theta = Phi(mu_theta_Phi) * 5;
  
  for (j in 1:n) {
    for( i in 1:n_cp) {

      pa[i, j] = inv_logit( theta[j] * (vsA[i,j] - vsB[i,j]) );
      log_lik[i, j] = bernoulli_logit_lpmf( co[i,j] | theta[j] * (vsA[i,j] - vsB[i,j]) );
      
    }
  }
}