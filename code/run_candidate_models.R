run_candidate_models <- function(memdat_long){

  res <- list(
    "Common lambda btw age groups" = tryCatch({fit_mod_common_lambda(memdat_long)}, error = function(e) 1), 
    "Common linear retest effect btw age groups" = tryCatch({fit_mod_common_linear_retests(memdat_long)}, error = function(e) 1), 
    "Common retest effect btw age groups" = tryCatch({fit_mod_common_retests(memdat_long)}, error = function(e) 1), 
    "Common retest effect and lambda btw age groups" = tryCatch({fit_mod_common_retests_lambda(memdat_long)}, error = function(e) 1), 
    "Common single retest effect btw age groups" = tryCatch({fit_mod_common_single_retests(memdat_long)}, error = function(e) 1), 
    "Common single retest effect btw age groups and drop sex" = tryCatch({fit_mod_common_single_retests_nosex(memdat_long)}, error = function(e) 1), 
    "Common training effect btw age groups" = tryCatch({fit_mod_common_trainings(memdat_long)}, error = function(e) 1), 
    "Full model" = tryCatch({fit_mod_full(memdat_long)}, error = function(e) 1), 
    "Linear retest effect" = tryCatch({fit_mod_linear_retests(memdat_long)}, error = function(e) 1), 
    "Linear training effect" = tryCatch({fit_mod_linear_trainings(memdat_long)}, error = function(e) 1), 
    "No exponential" = tryCatch({fit_mod_noexp(memdat_long)}, error = function(e) 1), 
    "Drop age deviation" = tryCatch({fit_mod_noage(memdat_long)}, error = function(e) 1), 
    "Drop sex" = tryCatch({fit_mod_nosex(memdat_long)}, error = function(e) 1), 
    "Replace age with time" = tryCatch({fit_mod_time(memdat_long)}, error = function(e) 1)
  )
  
  # Check which failed
  failed_runs <- map_lgl(res, ~ is.numeric(.x))
  if(sum(failed_runs) > 0){
    message("Running the following models failed:\n", 
            paste(names(res)[failed_runs], collapse = "\n"))
  } else {
    message("All models ran successfully\n")
  }
  

  res[!failed_runs]
}
