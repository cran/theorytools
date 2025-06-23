## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(theorytools)

## ----eval = FALSE, echo = TRUE, class.source = 'fold-hide'--------------------
# # FAIR theory: The Empirical Cycle
# 
# ## Description
# 
# This is a FAIR implementation of De Groot and Spiekerman's "empirical cycle"
# theory, a theory of cumulative knowledge acquisition through scientific
# research.
# 
# ## Interoperability
# 
# The theory is implemented in the DOT language for describing graphs.
# 
# ### Rendering the theory using graphviz
# 
# See the graphviz manual for more information: https://graphviz.org
# 
# ## Contributing
# 
# If you want to contribute to this project, please get involved. You can do so in
# three ways:
# 
# 1. **To discuss the current implementation and discuss potential changes**, file
# a ‘GitHub’ issue [here](https://github.com/cjvanlissa/empirical_cycle/issues)
# 2. **To directly propose changes**, send a pull request containing the proposed
# changes [here](https://github.com/cjvanlissa/tidySEM/pulls)
# 3. **To create a derivative theory**, please fork the repository
# [here](https://github.com/cjvanlissa/empirical_cycle/fork). Please cite this
# repository (see below), and add this repository as a related work (below and by
# adding the appropriate metadata on Zenodo).
# 
# By participating in this project, you agree to abide by the [Contributor
# Covenant](https://www.contributor-covenant.org/version/2/0/code_of_conduct.html).
# 
# ## Related Works
# 
# See this project's Zenodo page for cross-references to related work.
# 
# This repository contains an implementation of the "empirical cycle", a model
# proposed by De Groot and Spiekerman (1969, p. 28)
# 
# > De Groot, A. D., & Spiekerman, J. A. A. (1969). Methodology: Foundations of
# > inference and research in the behavioral sciences. De Gruyter Mouton.
# > https://doi.org/10.1515/9783112313121
# 
# ## Citing this work
# 
# See [this project's Zenodo page](https://doi.org/10.5281/zenodo.14552329) for
# the preferred citation.

