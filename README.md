This repo contains some reanalysis of the data used in [Maglich _et al_ (2014)](https://scholar.google.com/scholar?hl=en&as_sdt=0%2C7&q=More+than+just+hormones%3A+H295R+cells+as+predictors+of+reproductive+toxicity&btnG=), "More than just hormones: H295R cells as predictors of reproductive toxicity."

The original data and code are in [https://github.com/topepo/steroidogenic_tox](https://github.com/topepo/steroidogenic_tox). This repo is a "before" version: it was made prior to effective tooling, mostly developed by RStudio/Posit. 

This repo is the "after" version, which takes a more deliberate and modern approach to a data analysis project. The "wtf" in the repo name is a nod to the "What They Forgot to Teach You About R" materials at [https://github.com/rstats-wtf](https://github.com/rstats-wtf).

To reproduce the results, restore the R packages using

```r
library(renv)
restore()
status()
install()
```

and then run the files, in order, in the `code` directory. 