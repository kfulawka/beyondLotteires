functions {
  
  // probability weighting functino
  real pwf(real p, real gam, real del) {
    
    real wp;
    
    wp = (del*p^gam) / ( del*p^gam + (1-p)^gam ); 
    
    return wp;
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
  int max_a;
}

parameters {
  
  real mu_del_Phi;
  real<lower = 0> sigma_del;
  vector[n] del_Phi;
  
  real mu_gam_Phi;
  real<lower = 0> sigma_gam;
  vector[n] gam_Phi;
  
  real mu_aw_Phi;
  real<lower = 0> sigma_aw;
  vector[n] aw_Phi;
  
  real mu_theta_Phi;
  real<lower = 0> sigma_theta;
  vector[n] theta_Phi;
  
}

transformed parameters {
  
  // subjective values and weights
  matrix[n_cp, n] vXa;
  matrix[n_cp, n] wXa1;
  matrix[n_cp, n] wXa2;
  matrix[n_cp, n] vsA;
  
  matrix[n_cp, n] vXb;
  matrix[n_cp, n] wXb1;
  matrix[n_cp, n] wXb2;
  matrix[n_cp, n] vsB;
  
  matrix[n_cp, n] max_d_ar;
  
  // parameters on wanted scales
  vector[n] aw;
  vector[n] del;
  vector[n] gam;
  vector[n] theta;
  
  // scaling
  aw = Phi_approx(mu_aw_Phi + sigma_aw * aw_Phi);
  gam = Phi_approx(mu_gam_Phi + sigma_gam * gam_Phi);
  del = exp(mu_del_Phi + sigma_del * del_Phi);
  theta = exp(mu_theta_Phi + sigma_theta * theta_Phi);
  
  // CPT
  for (j in 1:n) {
    for (i in 1:n_cp) {
      
      max_d_ar[i,j] = max([fabs(xA[i,j]), fabs(xB[i,j])])/max_a;
      
      // option A
      vXa[i,j] = s * fabs(xA[i,j]);
      
      wXa1[i,j] = pwf(p[i,1], gam[j], del[j]);
      wXa2[i,j] = pwf(p[i,1], 1 - max_d_ar[i,j], max_d_ar[i,j] * 10);

      vsA[i,j] = vXa[i,j] * (wXa2[i,j]*aw[j] + wXa1[i,j]*(1 - aw[j]));
      
      // option B
      vXb[i,j] = s * fabs(xB[i,j]);       

      wXb1[i,j] = pwf(p[i,2], gam[j], del[j]);
      wXb2[i,j] = pwf(p[i,2], 1 - max_d_ar[i,j], max_d_ar[i,j] * 10);

      vsB[i,j] = vXb[i,j] * (wXb2[i,j]*aw[j] + wXb1[i,j]*(1 - aw[j]));
      
    }
  }
}

model {

  mu_aw_Phi ~ std_normal();
  sigma_aw ~ normal(.5, .13);

  mu_gam_Phi ~ std_normal();
  sigma_gam ~ normal(.5, .13);
  
  mu_del_Phi ~ std_normal();
  sigma_del ~ normal(.5, .13);
  
  mu_theta_Phi ~ std_normal();
  sigma_theta ~ normal(.5, .13);
  
  // individual parameters (vectorized)
  aw_Phi ~ std_normal();
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

  real mu_aw = Phi_approx(mu_aw_Phi);
  real mu_gam = Phi_approx(mu_gam_Phi);
  real mu_del = exp(mu_del_Phi);
  real mu_theta = exp(mu_theta_Phi);
  
  for (j in 1:n) {
    for( i in 1:n_cp) {
      
      pa[i, j] = inv_logit( theta[j] * (vsA[i,j] - vsB[i,j]) );
      
      log_lik[i, j] = bernoulli_logit_lpmf( co[i,j] | theta[j] * (vsA[i,j] - vsB[i,j]) );
      
    }
  }
}