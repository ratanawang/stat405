#log_joint <- function(beta_0, beta_i, beta_w, beta_h, y, intersection, weekend, heavy) {
log_joint <- function(beta_0, beta_i, beta_w, y, intersection, weekend) {
  # log prior
  log_prior_beta_0 <- dnorm(beta_0, mean = 0, sd = 5, log = TRUE)
  log_prior_beta_i <- dnorm(beta_i, mean = 0, sd = 2, log = TRUE)
  log_prior_beta_w <- dnorm(beta_w, mean = 0, sd = 1, log = TRUE)
  #log_prior_beta_h <- dnorm(beta_h, mean = 0, sd = 1, log = TRUE)
  
  log_prior <- log_prior_beta_0 + log_prior_beta_i + log_prior_beta_w# + log_prior_beta_h
  
  log_odds <- beta_0 + (beta_i * intersection) + (beta_w * weekend)# + (beta_h * heavy)
  
  # log likelihood
  log_likelihood = sum(dbinom(y, size = 1, prob = plogis(log_odds), log = TRUE))
  
  log_prior + log_likelihood
}



#custom_mh <- function(y, intersection, weekend, heavy, n_iters, beta_0, beta_i, beta_w, beta_h) {
custom_mh <- function(y, intersection, weekend, n_iters, beta_0, beta_i, beta_w) {
  start_time <- Sys.time()
  
  beta_0_trace <- rep(NA, n_iters)
  beta_i_trace <- rep(NA, n_iters)
  beta_w_trace <- rep(NA, n_iters)
  #beta_h_trace <- rep(NA, n_iters)
  
  curr_beta_0 <- beta_0
  curr_beta_i <- beta_i
  curr_beta_w <- beta_w
  #curr_beta_h <- beta_h
  
  curr_log_joint <- log_joint(curr_beta_0, curr_beta_i, curr_beta_w, y, intersection, weekend)
  #curr_log_joint <- log_joint(curr_beta_0, curr_beta_i, curr_beta_w, curr_beta_h, y, intersection, weekend, heavy)
  
  acceptance_beta_0 <- 0
  acceptance_beta_i <- 0
  acceptance_beta_w <- 0
  #acceptance_beta_h <- 0
  
  sd_beta_0 <- 0.1
  sd_beta_i <- 0.1
  sd_beta_w <- 0.1
  #sd_beta_h <- 0.1
  
  for (i in 1:n_iters) {
    # kernel 1: propose a new beta_0
    
    beta_0_proposal <- curr_beta_0 + rnorm(n=1, mean=0, sd=sd_beta_0)
    
    #proposal_log_joint <- log_joint(beta_0_proposal, curr_beta_i, curr_beta_w, curr_beta_h, y, intersection, weekend, heavy)
    proposal_log_joint <- log_joint(beta_0_proposal, curr_beta_i, curr_beta_w, y, intersection, weekend)
    log_acceptance_prob <- proposal_log_joint - curr_log_joint
    
    if (log(runif(1)) < log_acceptance_prob) {
      curr_beta_0 <- beta_0_proposal
      curr_log_joint <- proposal_log_joint
      acceptance_beta_0 <- acceptance_beta_0 + 1
    }
    
    beta_0_trace[i] <- curr_beta_0
    
    
    
    # kernel 2: propose a new beta_i
    
    beta_i_proposal <- curr_beta_i + rnorm(n=1, mean=0, sd=sd_beta_i)
    
    #proposal_log_joint <- log_joint(curr_beta_0, beta_i_proposal, curr_beta_w, curr_beta_h, y, intersection, weekend, heavy)
    proposal_log_joint <- log_joint(curr_beta_0, beta_i_proposal, curr_beta_w, y, intersection, weekend)
    log_acceptance_prob <- proposal_log_joint - curr_log_joint
    
    if (log(runif(1)) < log_acceptance_prob) {
      curr_beta_i <- beta_i_proposal
      curr_log_joint <- proposal_log_joint
      acceptance_beta_i <- acceptance_beta_i + 1
    }
    
    beta_i_trace[i] <- curr_beta_i
    
    
    
    # kernel 3: propose a new beta_w
    
    beta_w_proposal <- curr_beta_w + rnorm(n=1, mean=0, sd=sd_beta_w)
    
    # proposal_log_joint <- log_joint(curr_beta_0, curr_beta_i, beta_w_proposal, curr_beta_h, y, intersection, weekend, heavy)
    proposal_log_joint <- log_joint(curr_beta_0, curr_beta_i, beta_w_proposal, y, intersection, weekend)
    log_acceptance_prob <- proposal_log_joint - curr_log_joint
    
    if (log(runif(1)) < log_acceptance_prob) {
      curr_beta_w <- beta_w_proposal
      curr_log_joint <- proposal_log_joint
      acceptance_beta_w <- acceptance_beta_w + 1
    }
    
    beta_w_trace[i] <- curr_beta_w
    
    
    # kernel 4: propose a new beta_h
    
    # beta_h_proposal <- curr_beta_h + rnorm(n=1, mean=0, sd=sd_beta_h)
    # 
    # proposal_log_joint <- log_joint(curr_beta_0, curr_beta_i, curr_beta_w, beta_h_proposal, y, intersection, weekend, heavy)
    # log_acceptance_prob <- proposal_log_joint - curr_log_joint
    # 
    # if (log(runif(1)) < log_acceptance_prob) {
    #   curr_beta_h <- beta_h_proposal
    #   curr_log_joint <- proposal_log_joint
    #   acceptance_beta_h <- acceptance_beta_h + 1
    # }
  }
  
  end_time <- Sys.time()
  
  
  return(
    list(
      beta_0_trace = beta_0_trace,
      beta_i_trace = beta_i_trace,
      beta_w_trace = beta_w_trace,
      #beta_h_trace = beta_h_trace,
      acceptance_beta_0 = acceptance_beta_0 / n_iters,
      acceptance_beta_i = acceptance_beta_i / n_iters,
      acceptance_beta_w = acceptance_beta_w / n_iters,
      #acceptance_beta_h = acceptance_beta_h / n_iters,
      runtime = as.numeric(end_time - start_time)
    )
  )
  
  
  
}