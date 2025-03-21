analysis_g_computation <-
  function(
    data,
    formula,
    family,
    estimand = c("difference", "ratio", "oddsratio")[1],
    treatment_column,
    se_method = c("none", "influence", "score", "bootstrap")[1],
    alpha = 0.05,
    bootstrap_parameters = emcee::analysis_bootstrap_parameters(),
    bootstrap_ci_method = "bca",
    verbose = FALSE
  ){
    all_vars <- all.vars(expr = formula)
    
    if(!all(all_vars %in% names(data))){
      stop(
        "`formula` contains variables not included in `data`: ",
        setdiff(x = all.vars(expr = formula), y = names(data))
      )
    }
    
    if(!treatment_column %in% all_vars){
      stop(
        "`treatment_column` (", treatment_column, ") must be in `formula` (",
        Reduce(f = paste, x = deparse(expr = formula)), ")"
      )
    }
    
    if(!all(data[, treatment_column] %in% 0:1)){
      stop(
        "`treatment_column` must be a binary indicator."
      )
    }
    
    if(!(estimand %in% c("difference", "ratio", "oddsratio"))){
      stop("`estimand` must be one of the following: \"difference\", ",
           "\"ratio\", or \"oddsratio\".")
    }
    
    if(!(se_method %in% c("none", "influence", "score", "bootstrap"))){
      stop("`se` must be one of the following: \"none\", ",
           "\"influence\", \"score\", or \"bootstrap\".")
    }
    
    if(se_method == "none"){
      return(
        analysis_g_computation_fit(
          data = data,
          formula = formula,
          family = family,
          estimand = estimand,
          treatment_column = treatment_column,
          verbose = verbose
        )
      )
    } else if(se_method %in% c("influence", "score")){
      if(estimand == "difference"){
        gc_fit <-
          analysis_g_computation_fit(
            data = data,
            formula = formula,
            family = family,
            estimand = estimand,
            treatment_column = treatment_column,
            verbose = TRUE
          )
        
        sigma_sq <-
          with(
            data = gc_fit,
            expr = {
              variance_factor*
                mean(
                  (
                    2*(2*a)*(y - y_pred) -
                      ((y1_pred - mean(y1_pred)) - (y0_pred - mean(y0_pred)))
                  )^2
                )/n_observations
            }
          )
        
        if(se_method == "influence"){
          se <- sqrt(sigma_sq)
        } else if(se_method == "score") {
          se <-
            sqrt(sigma_sq)*
            (1 - qnorm(p = 1 - alpha/2)/gc_fit$n_observations)^(-1/2)
        }
      } else {
        stop(
          "Standard errors using `", se_method, "` have not been implemented ",
          "for the estimand ", estimand
        )
      }
      
      gc_result <-
        stats::setNames(
          object = c(gc_fit$estimate, se,
                     gc_fit$estimate + c(-1, 1)*qnorm(p = c(1 - alpha/2))*se),
          nm = c("estimate", paste0("se_", se_method),
                 paste0(c("lcl", "ucl"), "_", se_method, "_", 1 - alpha))
          
        ) |> t() |>
        data.frame(
          estimand = estimand
        )
      
      if(verbose){
        return(
          c(
            result = gc_result,
            g_computation_fit = gc_fit,
            estimand = estimand,
            variance_factor = variance_factor
          )
        )
      } else{
        return(gc_result)
      }
      
    } else if(se_method %in% "bootstrap"){
      boot_result <-
        do.call(
          what = boot::boot,
          args =
            c(
              list(
                data = data,
                statistic =
                  function(data, indices, ...) {
                    analysis_g_computation_fit(
                      data = data[indices,],
                      ...
                    )
                  },
                formula = formula,
                family = family,
                estimand = estimand,
                treatment_column = treatment_column,
                verbose = FALSE
              ),
              bootstrap_parameters
            )
        )
      
      gc_fit <-
        analysis_g_computation_fit(
          data = data,
          formula = formula,
          family = family,
          estimand = estimand,
          treatment_column = treatment_column,
          verbose = TRUE
        )
      variance_factor <- gc_fit$variance_factor
      
      boot_result$t <-
        with(
          data = boot_result,
          mean(t) + (t - mean(t))*sqrt(variance_factor)
        )
      
      boot_result_ci <-
        boot::boot.ci(
          boot.out = boot_result,
          conf = 1 - alpha,
          type = bootstrap_ci_method
        )
      
      ci_methods <-
        setdiff(
          x = names(boot_result_ci),
          y = c("R", "t0", "call")
        )
      
      ci_results <- list()
      for(i in 1:length(ci_methods)){
        ci_i_unadj <-
          utils::tail(
            x = get(x = ci_methods[i], pos = boot_result_ci)[1,],
            n = 2
          )
        
        ci_i_adj <-
          boot_result$t0 +
          sqrt(variance_factor)*(ci_i_unadj - boot_result$t0)
        
        ci_results[[i]] <-
          stats::setNames(
            object =
              c(ci_i_unadj, ci_i_adj),
            nm =
              c(
                paste0(c("lcl", "ucl"), "_", ci_methods[i], "_", 1 - alpha),
                paste0(c("lcl", "ucl"), "_", ci_methods[i],
                       "_df_adjusted_", 1 - alpha)
              )
          )
      }
      
      boot_result_ci <-
        data.frame(
          estimate = as.numeric(boot_result$t0),
          se_boot = sd(boot_result$t),
          variance_factor = variance_factor,
          se_boot_df_adjusted = sd(boot_result$t*sqrt(variance_factor)),
          t(do.call(what = c, args = ci_results)),
          estimand = estimand
        )
      
      if(verbose){
        return(
          list(
            result = boot_result_ci,
            g_computation_fit = gc_fit,
            estimand = estimand,
            boot_object = boot_result
          )
        )
      } else {
        return(boot_result_ci)
      }
    }
  }

analysis_g_computation_fit <-
  function(
    data,
    formula,
    family,
    estimand = c("difference", "ratio", "oddsratio")[1],
    treatment_column,
    verbose = FALSE
  ){
    predictors <-
      all.vars(update.formula(old = formula, new = 0 ~ .))
    
    if(any(is.na(data[, predictors]))){
      # Impute any missing values using mean/mode imputation
      data <-
        impart::impute_covariates_mean_mode(
          data = data,
          baseline_covariates = predictors
        )
    }
    
    outcome_model <-
      stats::glm(
        formula = formula,
        family = family,
        data = data
      )
    
    y0_pred <-
      stats::predict(
        object = outcome_model,
        newdata =
          within(
            data = data,
            assign(
              x = treatment_column,
              value = 0
            )
          ),
        type = "response"
      )
    
    y1_pred <-
      stats::predict(
        object = outcome_model,
        newdata =
          within(
            data = data,
            assign(
              x = treatment_column,
              value = 1
            )
          ),
        type = "response"
      )
    
    # Estimate treatment effect
    if(estimand == "difference"){
      estimate = mean(y1_pred) - mean(y0_pred)
    } else if(estimand == "ratio"){
      estimate = mean(y1_pred)/mean(y0_pred)
      
    } else if(estimand == "oddsratio"){
      estimate <-
        (mean(y1_pred)/(1 - mean(y1_pred))) /
        (mean(y0_pred)/(1 - mean(y0_pred)))
    }
    
    return(
      if(verbose){
        n_observations <- length(outcome_model$residuals)
        
        n_covariates <-
          length(
            setdiff(
              x = attributes(terms(formula))$term.labels,
              y = treatment_column
            )
          )
        
        outcome <-
          all.vars(update.formula(old = formula, new = . ~ 0))
        
        y_pred <-
          stats::predict(
            object = outcome_model,
            newdata = data,
            type = "response"
          )
        
        list(
          estimate = estimate,
          y = data[, outcome],
          a = data[, treatment_column],
          y_pred = y_pred,
          y1_pred = y1_pred,
          y0_pred = y0_pred,
          estimand = estimand,
          n_observations = n_observations,
          n_covariates = n_covariates,
          variance_factor =
            (n_observations - 1)/(n_observations - n_covariates - 1)
        )
      } else {
        estimate
      }
    )
  }
