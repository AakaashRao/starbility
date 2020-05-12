starb_felm = function(spec, data, rhs, ...) {
  spec = as.formula(spec)
  model = lfe::felm(spec, data=data, weights=data$weight)
  r2 = summary(model)$r.squared
  model = model %>%
    broom::tidy()
  # (second option needed since felm appends a " `(fit)` " when doing IV)
  row = which(model$term==rhs | model$term==paste0('`',rhs,'(fit)`'))
  coef = model[row, 'estimate'] %>% as.numeric()
  se   = model[row, 'std.error'] %>% as.numeric()
  p    = model[row, 'p.value'] %>% as.numeric()

  z = qnorm(0.975)
  return(c(coef, p, coef+z*se, coef-z*se, r2))
}

