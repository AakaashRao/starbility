# starbility: coefficient stability plots under combinations of controls

A common exercise in applied microeconomics is to assess the stability of a coefficient under different choices of controls. This can be a tedious task for the econometrician, particularly with a large set of controls. More importantly, it's difficult to concisely convey this information to the reader. Regression tables are useful for displaying a limited number of models, but they're less useful for demonstrating how coefficient estimates evolve under dozens or even hundreds of sets of controls. `starbility` provides a simple interface to create a "coefficient stability plot" (also known as a "specification curve") allowing both the econometrician and the reader to assess coefficient stability under different combinations of controls. 

![alt text](https://github.com/AakaashRao/starbility/blob/master/doc/example.png)

`starbility` builds upon `lfe` and `ggplot2`, allowing for fast estimation of models with many groups of fixed effects and for flexible plotting. IV estimation, sample weights, clustered standard errors, and custom models are supported. See the [main vignette](https://htmlpreview.github.io/?https://github.com/AakaashRao/starbility/blob/master/doc/starbility.html) for an introduction to `starbility`, and see the [advanced vignette](https://htmlpreview.github.io/?https://github.com/AakaashRao/starbility/blob/master/doc/starbility-advanced.html) for a discussion of custom models, manual plotting, etc.

## Quick setup
```
devtools::install_github('https://github.com/AakaashRao/starbility')
library(starbility)
```

## Notes and acknowledgements 
Comments, criticism, suggestions, pull requests, etc. are very much appreciated. Email: <arao@g.harvard.edu>. 

Thanks to David Yanagizawa-Drott for suggesting the structure of the plot and to Ross Mattheis and Eric Karsten for very helpful discussions during development.

## Other implementations
Looking for a Stata implementation? Martin Andresen provides a package [here](https://github.com/martin-andresen/speccurve) and Hans H. Sievertsen provides a detailed example .ado file [here](https://github.com/hhsievertsen/speccurve). 

Ariel Ortiz Bobea provides an excellent alternative R implementation (with nicer aesthetics and easier customization) [here](https://github.com/ArielOrtizBobea/spec_chart). 
