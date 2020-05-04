starb_felm = function(spec, data, rhs) {
  spec = as.formula(spec)
  model = lfe::felm(spec, data=data, weights=data$weight) %>%
    broom::tidy()
  # (second option needed since felm appends a " `(fit)` " when doing IV)
  row = which(model$term==rhs | model$term==paste0('`',rhs,'(fit)`'))
  coef = model[row, 'estimate'] %>% as.numeric()
  se   = model[row, 'std.error'] %>% as.numeric()
  p    = model[row, 'p.value'] %>% as.numeric()

  return(c(coef, p, coef+1.96*se, coef-1.96*se))
}

