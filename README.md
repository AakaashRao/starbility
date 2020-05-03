# starbility: coefficient stability plots under combinations of controls

A common exercise in applied microeconomics is assessing the stability of a coefficient under different choices of controls. This can be a tedious task for the econometrician, particularly with a large set of controls, but more importantly, it's difficult to concisely convey this information to the reader. Regression tables are useful for displaying a limited number of models, but they're less useful for displaying how coefficients evolve under dozens or even hundreds sets of controls. \code{starbility} provides a simple interface to create a "coefficient stability plot" allowing both the econometrician and the reader to assess coefficient stability under different combinations of controls. 

![alt text](https://github.com/AakaashRao/starbility/blob/master/doc/example.png)

`starbility` builds upon `lfe` and `ggplot2`, allowing for fast estimation of models with many groups of fixed effects and for flexible plotting. See the [vignette](https://htmlpreview.github.io/?https://github.com/AakaashRao/starbility/blob/master/doc/starbility.html) for an introduction to `starbility`.

## Quick setup
```
devtools::install_github('https://github.com/AakaashRao/starbility')
library(starbility)
```

## Notes and Acknowledgements 
Comments, criticism, suggestions, pull requests, etc. are very much appreciated. Email: <arao@g.harvard.edu>. Thanks to David Yanagizawa-Drott for suggesting the structure of the plot.
