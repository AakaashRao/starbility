create_grid = function(perm, lhs, rhs, ...) {
  l = list(...)
  if (is.null(l$base)) base = c()
  if (is.null(l$perm_fe)) perm_fe = c()
  if (is.null(l$nonperm_fe)) nonperm_fe = c()
  if (is.null(l$cluster)) cluster = '0'
  l$temp = ''
  if (length(l)>0) {
    for(i in 1:length(l)) {
      assign(x = names(l)[i], value = l[[i]])
    }
  }
  if (is.null(l$iv)) {
    iv='0'
  } else {
    iv=paste0('(', rhs, '~', l$iv, ')')
  }
  if (is.null(l$include_no_fe_specs) | length(nonperm_fe)==0) include_no_fe_specs=T

  # Begin by creating grid of perm
  grid = expand.grid(rep(list(c(0,1)), length(perm)+length(perm_fe)))
  names(grid) = c(names(perm), names(perm_fe))

  # Add in base arguments
  base_matrix=data.frame(matrix(1L, nrow = nrow(grid), ncol = length(base)))
  names(base_matrix) = names(base)
  grid = cbind(base_matrix, grid)

  # Combined dictionary
  base_perm = c(base, perm)

  grid$expr = apply(grid[,1:length(base_perm)], 1, function(x) paste(base_perm[names(base_perm)[which(x==1)]], collapse='+'))
  if(length(perm_fe)>0) {
    grid$expr2 = apply(grid[,(length(base_perm)+1):ncol(grid)], 1, function(x) paste(perm_fe[names(perm_fe)[which(x==1)]], collapse='+'))
  } else {
    grid$expr2 = ''
  }
  # How many copies of grid do we need?

  # N of seq controls if no "none" option; n of seq controls + 1 if "none" option.
  # Now we add in the FEs

  if (include_no_fe_specs) {
    nonperm_fe = c(c('None'=''), nonperm_fe)
  }
  copies = length(nonperm_fe)
  fes = rep(nonperm_fe, each=nrow(grid))
  grid = do.call("rbind", replicate(copies, grid, simplify = FALSE))
  grid$fe = fes
  grid$expr2 = ifelse((grid$expr2=='' & grid$fe==''), '0', grid$expr2)
  grid$expr2 = ifelse((grid$expr2!='0' & grid$fe!=''), paste0(grid$expr2, '+'), grid$expr2)
  grid = grid %>% dplyr::mutate(expr = paste(expr, '|', expr2, fe, '|', iv, '|', cluster, sep=''))
  grid = grid[,c(names(base_perm), names(perm_fe), 'fe', 'expr')]
  grid$fe[grid$fe==''] = '0'
  grid$fe = factor(grid$fe, levels=unique(grid$fe))

  # If IV is not zero, the RHS should appear in the second part of the formula:
  if (iv=='0') {
    indices_to_append = which(substring(grid$expr, 1, 1)!='|')
    grid$expr[indices_to_append] = paste0('+', grid$expr[indices_to_append])
    grid$expr = paste(lhs, '~', rhs, grid$expr, sep='')
  } else { # Otherwise it should appear in the third part of the formula.
    # However, if the second part of the formula is blank, we need to
    # add a 0.
    indices_to_append = which(substring(grid$expr, 1, 1)=='|')
    grid$expr[indices_to_append] = paste0('0', grid$expr[indices_to_append])
    grid$expr = paste(lhs, '~', grid$expr, sep='')
  }
  return(grid)
}

estimate_single_model = function(data_, spec_, lhs_, rhs_) {
  model = broom::tidy(lfe::felm(as.formula(spec_),
                                data=data_, weights=data_$weight))
  row = which(model$term==rhs_ | model$term==paste0('`',rhs_,'(fit)`'))
  coef = model[row, 'estimate'] %>% as.numeric()
  se   = model[row, 'std.error'] %>% as.numeric()
  p    = model[row, 'p.value'] %>% as.numeric()
  return(c(coef, se, p))
}

create_model_estimates = function(data., lhs., rhs., perm., ...) {
  l = list(...)
  if (is.null(l$weights)) {
    data.$weight = 1
  } else {
    data.$weight = data.[[l$weights]]
  }

  grid = create_grid(perm=perm., lhs=lhs., rhs=rhs., ...) %>%
      mutate(model = purrr::map(expr, function(x) estimate_single_model(data., x, lhs., rhs.)),
             coef = purrr::map_dbl(model, function(x) x[1]),
             se = purrr::map_dbl(model, function(x) x[2]),
             p = purrr::map_dbl(model, function(x) x[3]),
             error_high = coef+1.96*se,
             error_low = coef-1.96*se) %>%
    mutate(model=row_number(),
           p = case_when(
             p<0.01 ~ 'p<0.01',
             0.01<=p & p<0.05 ~ 'p<0.05',
             0.05<=p & p<0.1 ~ 'p<0.10',
             p>0.1 ~ 'p>0.10'
           ))
  return(grid)
}

draw_plot = function(grid, perm., ...) {
  nmodels = max(grid$model)
  l = list(...)
  if (is.null(l$control_geom)) control_geom = 'rect'
  if (is.null(l$control_spacing)) {
    control_spacing = ifelse(nmodels>=40, 1, 0.75)
  }
  if (is.null(l$rel_height)) rel_height = 0.5
  if (is.null(l$point_size)) {
    point_size = case_when(
      nmodels<=10 ~ 3,
      10<nmodels & nmodels<=40 ~ 2,
      40<nmodels ~ 1
    )
  }
  if (is.null(l$error_alpha)) error_alpha = 0.2
  if (is.null(l$error_geom)) {
    error_geom = 'errorbar'
  }
  if (is.null(l$trim_top)) trim_top = -1
  if (is.null(l$control_text_size)) control_text_size = 9
  if (is.null(l$base)) base = c()
  if (is.null(l$perm_fe)) perm_fe = c()
  if (is.null(l$nonperm_fe)) nonperm_fe = c()
  if (is.null(l$rel_height)) rel_height = 0.25
  if (is.null(l$combine)) combine = T
  if (is.null(l$sort)) sort = 'none'

  if (length(l)>0) {
    for(i in 1:length(l)) {
      assign(x = names(l)[i], value = l[[i]])
    }
  }

  if (sort != 'none') {
    grid = grid %>% dplyr::select(-model)
    if (sort == 'asc') {
      grid = grid %>% dplyr::arrange(coef) %>%
        mutate(model = row_number())
    } else if (sort == 'desc') {
      grid = grid %>% dplyr::arrange(desc(coef)) %>%
        mutate(model = row_number())
    } else if (sort == 'asc-by-fe') {
      grid = grid %>%
        dplyr::arrange(fe, coef) %>%
        ungroup() %>%
        mutate(model = row_number())
    } else if (sort == 'desc-by-fe') {
      grid = grid %>%
        dplyr::arrange(fe, desc(coef)) %>%
        ungroup() %>%
        mutate(model = row_number())
    }
  }
  min_space = control_spacing/2

  coef_plot = ggplot2::ggplot(grid, aes(x = model, y = coef)) +
    geom_point(size=point_size, alpha=0.7, aes(col=p)) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size=12),
          axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank()) +
    guides(col=F) +
    scale_color_manual(breaks = c('p<0.01','p<0.05','p<0.10','p>0.10'),
      values=c('#F8766D', '#7CAE00', '#00BFC4', '#000000'))

  if (error_geom == 'ribbon') {
    coef_plot = coef_plot + geom_ribbon(aes(ymin=error_low, ymax=error_high), alpha=error_alpha)
  } else if (error_geom == 'errorbar') {
    coef_plot = coef_plot + geom_errorbar(aes(ymin=error_low, ymax=error_high), alpha=error_alpha)
  }

  if (!is.null(l$coef_ylim)) {
    coef_plot = coef_plot + coord_cartesian(ylim=coef_ylim, xlim=c(1-min_space, max(grid$model)+min_space))
  } else {
    coef_plot = coef_plot + coord_cartesian(xlim=c(1-min_space, max(grid$model)+min_space))
  }

  if (length(nonperm_fe)>0) {
    if (length(unique(grid$fe))>1) {
      fe_df = as.data.frame(model.matrix(~grid$fe-1))
    } else {
      fe_df = as.data.frame(rep(1, nrow(grid)))
    }
    if ('grid$fe0' %in% names(fe_df)) fe_df = fe_df %>% dplyr::select(-'grid$fe0')
    names(fe_df) = names(nonperm_fe)
    #fe_df = fe_df[,order(ncol(fe_df):1)]

    grid = grid %>% dplyr::bind_cols(fe_df)
  }
  rect_plot = grid %>% dplyr::select(names(base), names(perm.), names(perm_fe), names(nonperm_fe), -fe, model) %>%
    tidyr::gather(key, value, -model) %>%
    dplyr::mutate(value = as.factor(value),
           y = -as.numeric(factor(key, levels = unique(key))))
  control_plot = ggplot(rect_plot) +
    scale_fill_manual(values=c('#FFFFFF', '#000000')) +
    guides(fill=F) +
    theme_bw()  +
    theme(plot.margin=grid::unit(c(-trim_top,0,0,0), "lines"),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=control_text_size),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    scale_y_continuous(breaks = unique(rect_plot$y), labels = unique(rect_plot$key),
                       limits=c(min(rect_plot$y)-1, max(rect_plot$y)+1)) +
    coord_cartesian(xlim=c(1-min_space, max(grid$model)+min_space))

  if (control_geom == 'rect') {
    control_plot = control_plot +
      geom_rect(aes(xmin = model-min_space,
                    xmax = model+min_space,
                    ymin = y-control_spacing/2,
                    ymax = y+control_spacing/2,
                    fill=value), alpha=0.5)
  } else if (control_geom == 'circle') {
    control_plot = control_plot +
      ggforce::geom_circle(aes(x0 = model, y0 = y, fill = value, r=control_spacing/2), alpha=0.8)
  }

  if (combine) {
    return(combine_plots(coef_plot, control_plot, rel_height))
  } else {
    return (list(coef_plot, control_plot))
  }

}

#' Combine coefficient panel and control panel to create a stability plot.
#'
#' \code{combine_plots} is used to combined a coefficient panel and a control panel. When \code{combine=T}
#' in \code{create_stability_plot}, it is called automatically from \code{create_stability_plot}. However, it
#' can also be called by the user to bind two plots together. This is useful if the user wishes to edit one plot
#' using standard \code{ggplot2} syntax.
#'
#' @param coef A ggplot2 object. A coefficient plot, generally generated using \code{create_stability_plot}.
#' @param control A ggplot2 object. A control plot, generally generated using \code{create_stability_plot}.
#' @param rel_height A numeric scalar. Height of the control plot relative to the coefficient plot.
#' @return A \code{cowplot} grid.
#' @export
combine_plots = function(coef, control, rel_height=0.5, ...) {
  l = list(...)
  if (length(l)>0) {
    for(i in 1:length(l)) {
      assign(x = names(l)[i], value = l[[i]])
    }
  }
  return(cowplot::plot_grid(coef, control, nrow=2,
                            align = 'v', axis='b', rel_heights = c(1, rel_height)))
}

#' Create coefficient stability plot.
#'
#' \code{create_stability_plot} is used to produce a plot showing the stability of the OLS estimate
#' of the explanatory variable \code{rhs} on the outcome variable \code{lhs} under combinations
#' of a given set of controls. Fixed effects, clustering, weights, and instrumental
#' variables are supported.
#'
#' Each row of the bottom panel of the plot corresponds to a single variable set.
#' A variable set can contain one or more individual variables.
#' To include multiple variables in a single set, specify them in a single string, separated by '+'.
#'
#' @param data A dataframe containing the variables in the model will be estimated.
#' @param lhs A string indicating the name of the outcome variable in \code{data}.
#' @param rhs A string indicating the name of the explanatory variable for which coefficient estimates
#' will be plotted.
#' @param iv A string indicating the variables which should be used to instrument \code{rhs}.
#' If left unspecified, OLS coefficients are plotted.
#' @param perm A named dictionary in which values correspond to the sets of variables
#' that should be iterated upon to produce the stability plot and names correspond to the names
#' of these sets of variables that should be displayed in the plot.
#' @param base A named dictionary in which values correspond to the sets of variables that should
#' always be included in the model in all specifications and names correspond to the names
#' of these sets of variables that should be displayed in the plot.
#' @param perm_fe A named dictionary in which values correspond to the sets of fixed effects
#' that should be iterated upon to produce the stability plot and names correspond to the names
#' of these sets of variables that should be displayed in the plot. Functionally, these operate
#' identically to \code{perm}; the difference is that \code{starbility} uses \code{lfe} to sweep
#' them out of the normal equations, resulting in a performance boost over including them in
#' \code{perm}.
#' @param nonperm_fe A named dictionary in which values correspond to fixed effects that should be
#' iterated upon to produce the stability plot and names correspond to the names of these
#' sets of fixed effects that should be displayed in the plot. These fixed effects are included
#' sequentially in the plot, one at a time -- i.e. combinations of \code{nonperm_fe} are not included.
#' @param include_no_fe_specs A logical scalar. If one or more sets of fixed effects are
#' specified in \code{nonperm_fe}, should the plot additionally include estimates from models without
#' non-permuted fixed effects? Defaults to \code{T}.
#' @param sort A string specifying how models should be sorted by coefficient value. The default is
#' \code{none}, which preserves the order in which controls are permuted. Other options are
#' \code{asc} (sorted by ascending coefficient values), \code{desc} (sorted by descending
#' coefficient values), \code{asc-by-fe} (sorted by ascending coefficient values within non-permuted
#' fixed effects groups, but preserving the order of these groups), and \code{desc-by-fe}
#' (sorted by descending coefficient values within non-permuted fixed effects groups, but preserving the
#' order of these groups).
#' @param cluster A string indicating the name of the variable by which standard errors should be
#' clustered. Defaults to no clustering.
#' @param weights A string indicating the name of the variable containing weights. Defaults to equal
#' weighting.
#' @param point_size A numeric scalar indicating the size of the points indicating coefficient estimates.
#' Defaults to 1.
#' @param error_geom A string indicating the type of geom that should be used to indicate confidence
#' intervals on coefficient estimates. Currently supported are \code{ribbon}, \code{errorbar}, and \code{none}.
#' Defaults to \code{errorbar} if fewer than 100 models are plotted; defaults to \code{ribbon} if
#' 100 or more models are plotted.
#' @param error_alpha A numeric scalar indicating the alpha of the error geom. Defaults to 0.2.
#' @param coef_ylim A numeric vector of length two indicating the minimum and maximum values of the
#' y-axis in the coefficient plot. If not specified, uses \code{ggplot2} default.
#' @param control_geom A string indicating the geom that should be used to indicate the presence of
#' controls. Currently supported are \code{circle} and \code{rect}. Defaults to \code{rect}.
#' @param control_spacing A string indicating how large the geoms indicating the presence of controls
#' should be. For \code{control_geom=='circle'}, this is the diameter of the circle. For
#' \code{control_geom=='rect'}, this is the width of the rectangle. Defaults to 0.75 if fewer than
#' 40 models are displayed; defaults to 1 otherwise.
#' @param control_text_size A numeric scalar indicating how large the control name text
#' should be. Defaults to 9.
#' @param rel_height A numeric scalar indicating the size of the bottom panel (displaying presence of
#' controls) relative to the top panel (displaying presence of coefficients). Defaults to 0.25.
#' @param trim_top A numeric scalar indicating how close the bottom panel (displaying presence of
#' controls) should be to the top panel (displaying presence of coefficients). Useful when dealing with
#' large CIs.
#' @param combine A logical scalar. Return the panels combined as a single object, or return a list
#' containing the two panels separately?
#' @return A ggplot2 object.
#' @export
create_stability_plot = function(data, lhs, rhs, perm, ...) {
  grid = create_model_estimates(data. = data,
                      lhs. = lhs,
                      rhs. = rhs,
                      perm. = perm,
                      ...)
  plot = draw_plot(grid, perm. = perm, ...)
  return (plot)
}

