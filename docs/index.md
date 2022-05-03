## Conformal inference prediction regions for Multivariate response regression

This repository contains the R package [conformalInference.multi](https://cran.r-project.org/web/packages/conformalInference.multi/index.html) (now available also on CRAN), which can produce valid prediction regions at levels 1-α or 1-2α under the basic assumption of _i.i.d._ regression data. 

The package was developed as part of my MSc. final thesis in Mathematical Engineering at Politecnico di Milano, as a multivariate extension of the main methods for Conformal Prediction for regression in the univariate response case.

### Code Structure

There are three main famililies of functions:

- Prediction methods
- Regression methods
- Plot methods


The central idea upon which the package is designed is the following: regression methods **should not** be included into the prediction methods themselves. Final users can pass as input to the prediction methods custom-coded regression algorithms, which may be more suitable for the prediction task at hand. Anyways the most common regression methods are implemented in the package.

### Main Functions

<br/>
<div align="center">


| Syntax      | Description |
| ----------- | ---------------- |
|conformal.multidim.full| Computes Full Conformal prediction regions|
|conformal.multidim.jackplus | Computes Jackknife+ prediction regions|
|conformal.multidim.split| Computes Split Conformal prediction regions|
|conformal.multidim.msplit| Computes Multi Split Conformal prediction regions|
|elastic.funs| Build elastic net regression|
|lasso.funs| Build lasso regression|
|lm_multi| Build linear regression|
|mean_multi| Build regression functions with mean|
|plot_multidim| Plot the output of prediction methods|
|ridge.funs| Build elastic net regression|
  
  </div>


### Detailed description

A complete description of the theory underpinning the package, an analysis of all the main functions as well as a case study is presented in my final MSc. thesis paper, availble at the following [link]().

### Acknownledgments

Prof. Simone Vantini - _Politecnico di Milano_

Doct. Jacopo Diquigiovanni

Doct. Matteo Fontana

Prof. Aldo Solari - _Università Bicocca di Milano_

