## Conformal inference prediction regions for Multivariate response regression

This repository contains the package [conformalInference.multi] (now available also on CRAN), which can produce valid prediction regions at levels 1-α or 1-2α under the basic assumption of _i.i.d._ regression data. 

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




You can use the [editor on GitHub](https://github.com/paolo-vergo/conformalInference.multi/edit/master/docs/index.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [Basic writing and formatting syntax](https://docs.github.com/en/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/paolo-vergo/conformalInference.multi/settings/pages). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://support.github.com/contact) and we’ll help you sort it out.
