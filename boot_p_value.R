boot_p_value <-
  function(
    boot_object,
    ci_method = "bca",
    index = 1,
    null_value = 0,
    var_adjust = 1,
    alpha_max = 1,
    alpha_min = 10^-5,
    tolerance = 0.001,
    max_evaluations = 40,
    verbose = FALSE
  ){
    if(length(ci_method) > 1){
      stop(
        "Only one type of confidence interval should be specified."
      )
    }
    
    converged <- FALSE
    continue <- TRUE
    i <- n_rejected <- n_fail_to_reject <- 0
    all_ci_results <-
      data.frame(
        alpha = rep(NA, max_evaluations),
        lcl = NA,
        ucl = NA,
        rejected = NA
      )
    current_min <- alpha_min
    current_max <- alpha_max
    current_alpha <- mean(c(alpha_max, alpha_min))
    while(continue) {
      i <- i + 1
      
      all_ci_results$alpha[i] <- current_alpha
      
      ci_result <-
        boot::boot.ci(
          boot.out = boot_object,
          conf = 1 - current_alpha,
          type = ci_method,
          index = index
        )
      
      ci_method_name <-
        setdiff(
          x = names(ci_result),
          y = c("R", "t0", "call")
        )
      
      ci_unadjusted <- 
        utils::tail(x = ci_result[[ci_method_name]][1,], n = 2)
      
      if(var_adjust == 1) {
        ci_adjusted <- ci_unadjusted
      } else {
        ci_adjusted <-
          boot_object$t0 +
          sqrt(var_adjust)*(ci_unadjusted - boot_object$t0)
      }
      
      all_ci_results[i, c("lcl", "ucl")] <- ci_adjusted
      
      rejected <- all_ci_results$rejected[i] <-
       (null_value < ci_adjusted[1]) | (null_value > ci_adjusted[2])
      
      if(rejected) {
        # Decrease Alpha
        n_rejected <- n_rejected + 1
        current_max <- min(c(current_alpha, current_max))
        current_alpha <-
          current_alpha - c(current_alpha - current_min)/2
      } else {
        # Increase Alpha
        n_fail_to_reject <- n_fail_to_reject + 1
        current_min <- max(c(current_alpha, current_min))
        current_alpha <-
          current_alpha + c(current_max - current_alpha)/2
      }
      
      # Evaluate convergence
      if(i > 1){
        diff_alpha <- abs(diff(all_ci_results$alpha[c(i-1, i)]))
        if(n_fail_to_reject > 4 & n_rejected > 4 & diff_alpha <= tolerance) {
          converged <- TRUE
          continue <- FALSE
        }
      }
      if(i >= max_evaluations){
        continue <- FALSE
        warning("Iteration limit reached without convergence.")
      }
    }
    boot_p_value <- min(subset(all_ci_results, rejected)$alpha)
    if(verbose){
      return(
        list(
          boot_p_value = boot_p_value,
          converged = converged,
          boot_object = boot_object,
          ci_method = ci_method,
          index = index,
          var_adjust = var_adjust,
          alpha_max = alpha_max,
          alpha_min = alpha_min,
          tolerance = tolerance,
          max_evaluations = 40,
          n_evaluations = i,
          all_ci_results = all_ci_results
        )
      )
    } else {
      return(boot_p_value)
    }
  }