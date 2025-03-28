functions {
  
  // probability weighting function
  real pwf(real p, real gam, real del) {
    
    real wp;
    
    wp = (del*p^gam) / ( del*p^gam + (1-p)^gam ); 
    
    return wp;
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
  matrix[n_cp, 2] p;
  int<lower = 0, upper = 1> co[n_cp, n];
  int s;
}

parameters {
  
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
  
  // parameters on wanted scales
  vector[n] del;
  vector[n] gam;
  vector[n] theta;
  
  // scaling
  gam = Phi(mu_gam_Phi + sigma_gam * gam_Phi);
  del = Phi(mu_del_Phi + sigma_del * del_Phi) * 10;
  theta = Phi(mu_theta_Phi + sigma_theta * theta_Phi) * 5;
  
  // CPT
  for (j in 1:n) {
    for (i in 1:n_cp) {
      
      // option A
      vXa[i,j] = vf(s, xA[i,j], 1);
      
      wXa[i,j] = pwf(p[i,1], gam[j], del[j]);
      
      vsA[i,j] = vXa[i,j] * wXa[i,j];
      
      // option B
      vXb[i,j] = vf(s, xB[i,j], 1);       
      
      wXb[i,j] = pwf(p[i,2], gam[j], del[j]);
      
      vsB[i,j] = vXb[i,j] * wXb[i,j];

    }
  }
}

model {
  
  mu_gam_Phi ~ std_normal();
  sigma_gam ~ normal(.5, .13);
  
  mu_del_Phi ~ std_normal();
  sigma_del ~ normal(.5, .13);
  
  mu_theta_Phi ~ std_normal();
  sigma_theta ~ normal(.5, .13);
  
  // individual parameters (vectorized)
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