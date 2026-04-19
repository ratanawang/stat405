log_joint <- function(beta_0, beta_i, beta_w, y, intersection, weekend) {
  # log prior
  log_prior_beta_0 <- dnorm(beta_0, mean = 0, sd = 5, log = TRUE)
  log_prior_beta_i <- dnorm(beta_i, mean = 0, sd = 2, log = TRUE)
  log_prior_beta_w <- dnorm(beta_w, mean = 0, sd = 1, log = TRUE)
  
  log_prior <- log_prior_beta_0 + log_prior_beta_i + log_prior_beta_w
  
  log_odds <- beta_0 + (beta_i * intersection) + (beta_w * weekend)
  
  # log likelihood
  log_likelihood = sum(dbinom(y, size = 1, prob = plogis(log_odds), log = TRUE))
  
  log_prior + log_likelihood
}



custom_mh <- function(y, intersection, weekend, n_iters, beta_0, beta_i, beta_w) {
  start_time <- Sys.time()
  
  beta_0_trace <- rep(NA, n_iters)
  beta_i_trace <- rep(NA, n_iters)
  beta_w_trace <- rep(NA, n_iters)
  
  curr_beta_0 <- beta_0
  curr_beta_i <- beta_i
  curr_beta_w <- beta_w
  
  curr_log_joint <- log_joint(curr_beta_0, curr_beta_i, curr_beta_w, y, intersection, weekend)
  
  acceptance_beta_0 <- 0
  acceptance_beta_i <- 0
  acceptance_beta_w <- 0
  
  sd_beta_0 <- 0.1
  sd_beta_i <- 0.1
  sd_beta_w <- 0.1
  
  for (i in 1:n_iters) {
    # kernel 1: propose a new beta_0
    
    beta_0_proposal <- curr_beta_0 + rnorm(n=1, mean=0, sd=sd_beta_0)
    
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
    
    proposal_log_joint <- log_joint(curr_beta_0, curr_beta_i, beta_w_proposal, y, intersection, weekend)
    log_acceptance_prob <- proposal_log_joint - curr_log_joint
    
    if (log(runif(1)) < log_acceptance_prob) {
      curr_beta_w <- beta_w_proposal
      curr_log_joint <- proposal_log_joint
      acceptance_beta_w <- acceptance_beta_w + 1
    }
    
    beta_w_trace[i] <- curr_beta_w
  }
  
  end_time <- Sys.time()
  
  
  return(
    list(
      beta_0_trace = beta_0_trace,
      beta_i_trace = beta_i_trace,
      beta_w_trace = beta_w_trace,
      acceptance_beta_0 = acceptance_beta_0 / n_iters,
      acceptance_beta_i = acceptance_beta_i / n_iters,
      acceptance_beta_w = acceptance_beta_w / n_iters,
      runtime = as.numeric(end_time - start_time),
      last_beta_0 = curr_beta_0,
      last_beta_i = curr_beta_i,
      last_beta_w = curr_beta_w
    )
  )
  
  
  
}


forward <- function(synthetic_data_size, df) {
  b0 <- rnorm(1, mean = 0, sd = 5)
  bi <- rnorm(1, mean = 0, sd = 2)
  bw <- rnorm(1, mean = 0, sd = 1)
  
  # we will take random samples from the crash dataframe
  
  df_subset <- df |>
    slice_sample(n = synthetic_data_size)
  
  log_odds <- b0 + (bi * df_subset$Intersection.Crash) + (bw * df_subset$weekend)
  
  y_synthetic <- rbinom(n = synthetic_data_size, size = 1, prob = plogis(log_odds))
  
  
  list(
    y = y_synthetic,
    intersection = df_subset$Intersection.Crash,
    weekend = df_subset$weekend,
    beta_0 = b0,
    beta_i = bi,
    beta_w = bw
  )
}


forward_posterior <- function(synthetic_data_size, n_mcmc_iters, df, param) {
  initial <- forward(synthetic_data_size, df)
  
  if (n_mcmc_iters > 0) {
    samples <- custom_mh(y = initial$y,
                         intersection = initial$intersection,
                         weekend = initial$weekend,
                         n_iters = n_mcmc_iters,
                         beta_0 = initial$beta_0,
                         beta_i = initial$beta_i,
                         beta_w = initial$beta_w)
    
    list(
      beta_0 = samples$last_beta_0,
      beta_i = samples$last_beta_i,
      beta_w = samples$last_beta_w
    )[[param]]
    
                         
  }
  
  list(
    beta_0 = initial$beta_0,
    beta_i = initial$beta_i,
    beta_w = initial$beta_w
  )[[param]]
}