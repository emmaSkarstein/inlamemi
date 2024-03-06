#' List the survival likelihoods in INLA
#'
#' @return List of survival models in INLA
inla_survival_families <- function(){
  inla_families <- names(INLA::inla.models()$likelihood)
  surv_families <- inla_families[grepl("surv", inla_families)]
  surv_with_dots <- gsub("surv", ".surv", surv_families)
  return(c(surv_families, surv_with_dots, "coxph"))
}
