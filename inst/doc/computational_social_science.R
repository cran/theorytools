## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

## ----eval = requireNamespace("webexercises", quietly = TRUE)------------------
webexercises::mcq(sample(c(answer = "Executing code immediately in an R session that a human user is interacting with.", "Having one function call another function; these functions are said to be 'interacting'", "Evaluating code by placing it in an Rmarkdown document, and knitting that document.")))

## ----eval = FALSE-------------------------------------------------------------
# install.packages("theorytools", prompt = FALSE)
# install.packages("dagitty", prompt = FALSE)
# install.packages("tidySEM", prompt = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# library(theorytools)
# library(dagitty)
# library(tidySEM)

## ----download_theory----------------------------------------------------------
theorytools::download_theory("10.5281/zenodo.15648655", path = "theory")

## ----eval = requireNamespace("webexercises", quietly = TRUE)------------------
webexercises::mcq(sample(c(answer = "To prevent overwriting the WORCS project's README and LICENSE files", "To avoid downloading unnecessary files", "Because theorytools only works inside a 'theory/' directory", "Because dagitty requires theory files to be placed in a specific folder")))

## ----load_theory, eval = FALSE------------------------------------------------
# sdt <- dagitty::dagitty(paste(readLines("../theory/sdt.txt"), collapse = "\n"))

## ----echo = FALSE-------------------------------------------------------------
sdt <- structure("dag {\nexternal_event\nhealthy_development\nintegration\nintrinsic_motivation\nlocus_of_causality\nneeds\nwellbeing\nexternal_event -> intrinsic_motivation\nexternal_event -> locus_of_causality\nexternal_event -> needs\nintegration -> healthy_development\nintegration -> wellbeing\nintrinsic_motivation -> healthy_development\nintrinsic_motivation -> wellbeing\nlocus_of_causality -> intrinsic_motivation\nneeds -> integration\nneeds -> intrinsic_motivation\nneeds -> wellbeing\n}\n", class = "dagitty")


## ----echo = TRUE, eval = FALSE------------------------------------------------
# tidySEM::graph_sem(sdt)

## ----echo = FALSE, out.width="60%"--------------------------------------------
if(!file.exists('sdt.png')){
  set.seed(1)
  p <- tidySEM::graph_sem(sdt, rect_width = 2, rect_height = 2) + ggplot2::scale_x_continuous(expand = c(.2,0))
  ggplot2::ggsave("sdt.png", p, device = "png")
}
knitr::include_graphics("sdt.png")

## ----eval = requireNamespace("webexercises", quietly = TRUE)------------------
webexercises::mcq(sample(c(answer = "integration, intrinsic_motivation, needs", "integration, intrinsic_motivation", "needs, intrinsic_ _motivation")))

## ----simulate-data, eval = FALSE----------------------------------------------
# set.seed(1)
# theorytools::simulate_data(sdt, n = 5)

## ----echo = FALSE-------------------------------------------------------------
set.seed(1)
knitr::kable(theorytools::simulate_data(sdt, n = 5), digits = 2)

## ----eval = requireNamespace("webexercises", quietly = TRUE)------------------
webexercises::mcq(sample(c(answer = "Fitting a model to real-world data", "Exploring what-if scenarios", "Conducting power analysis", "Preregistering a reproducible analysis pipeline")))

## -----------------------------------------------------------------------------
sdt_pruned <- theorytools:::prune_dag(sdt,
                                      exposure = "intrinsic_motivation",
                                      outcome = "wellbeing")
sdt_pruned

## ----eval = FALSE-------------------------------------------------------------
# set.seed(1)
# df <- theorytools::simulate_data(sdt_pruned, n = 100)
# head(df)

## ----echo = FALSE-------------------------------------------------------------
set.seed(1)
df <- theorytools::simulate_data(sdt_pruned, n = 100)
knitr::kable(head(df), digits = 2)

## ----analysis-----------------------------------------------------------------
res <- lm(wellbeing ~ intrinsic_motivation + needs, data = df)
summary(res)

## ----eval = FALSE-------------------------------------------------------------
# sum_res <- summary(res)
# sum_res$coefficients["intrinsic_motivation", "Pr(>|t|)"] < .05

## ----eval = requireNamespace("webexercises", quietly = TRUE)------------------
webexercises::torf(TRUE)

## ----install_targets, results='hide', eval = FALSE----------------------------
# install.packages("targets", prompt = FALSE)
# install.packages("tarchetypes", prompt = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# worcs::add_targets()

## ----eval = FALSE-------------------------------------------------------------
# list(
#   tar_target(
#     name = data,
#     command = tibble(x = rnorm(100), y = rnorm(100))
#   ),
#   tar_target(
#     name = model,
#     command = coefficients(lm(y ~ x, data = data))
#   ),
#   tarchetypes::tar_render(manuscript, "manuscript/manuscript.Rmd")
# )

## ----simulate_data------------------------------------------------------------
theorytools::simulate_data(sdt_pruned, n = 100, run = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# writeLines(
#   theorytools::simulate_data(sdt_pruned, n = 100, run = FALSE),
#   "R/generate_data.R"
# )

## -----------------------------------------------------------------------------
expand.grid(
  beta = c(.1, .2, .4),
  n = c(50, 100, 200)
)

## ----eval = FALSE-------------------------------------------------------------
# install.packages("visNetwork", prompt = FALSE)
# targets::tar_visnetwork()

## ----echo = FALSE-------------------------------------------------------------
knitr::include_graphics("visnetwork.png")

## ----eval = FALSE-------------------------------------------------------------
# knitr::kable(study_results, digits = 2)

## ----echo = FALSE-------------------------------------------------------------
study_results = structure(list(beta = c(0, 0.2, 0.4, 0, 0.2, 0.4, 0, 0.2, 0.4
), n = c(50, 50, 50, 100, 100, 100, 200, 200, 200), power = c(0.09, 
0.22, 0.77, 0.06, 0.56, 0.98, 0.03, 0.74, 1)), class = "data.frame", row.names = c(NA, 
-9L))
knitr::kable(study_results, digits = 2)

## -----------------------------------------------------------------------------
library(ggplot2)
df_plot <- study_results
df_plot$beta <- ordered(df_plot$beta)
ggplot(df_plot, aes(x = n, y = power, color = beta, shape = beta)) +
  geom_point() +
  geom_line() +
  theme_bw()

## ----eval = FALSE-------------------------------------------------------------
# # Snapshot the current state of the endpoints
# worcs::snapshot_endpoints()
# # Destroy the cache of targets results
# targets::tar_destroy()
# # worcs::reproduce() calls targets::tar_make(), then worcs::check_endpoints()
# worcs::reproduce()

## ----eval = FALSE-------------------------------------------------------------
# install.packages("future", prompt = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# library(future)
# plan(multisession, workers = 4L)

## ----eval = FALSE-------------------------------------------------------------
# library(future)
# plan(multisession, workers = parallelly::availableCores()-2L)

## ----eval = FALSE-------------------------------------------------------------
# future.apply::future_replicate(n = reps, expr = {
#       df <- with(as.list(thisrow), generate_data(beta = beta, n = n))
#       analyze_data(df)
#     },
#     future.seed = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# perform_study <- function(study_design, reps = 100){
#   # For each row of the study design, execute a function
#   pwr <- apply(study_design, 1, function(thisrow){
#     # Replicate the row of the study design reps times
#     out <- replicate(n = reps, expr = {
#       # Simulate data with the beta and n from thisrow
#       df <- with(as.list(thisrow), generate_data(beta = beta, n = n))
#       # Analyze those data
#       analyze_data(df)
#     })
#     # Calculate the proportion of significant results using mean()
#     mean(out)
#   })
#   # Make a data frame containing the study design and study results (pwr)
#   data.frame(study_design, power = pwr)
# }

## ----eval = FALSE-------------------------------------------------------------
# perform_study <- function(study_design, reps = 100){
#   library(future)
#   # Sets up clusters from number of cores
#   plan(multisession, workers = parallelly::availableCores()-2L)
#   pwr <- apply(study_design, 1, function(thisrow){
#     # Replicate the row of the study design reps times
#     out <- future.apply::future_replicate(n = reps, expr = {
#       # Simulate data with the beta and n from thisrow
#       df <- with(as.list(thisrow), generate_data(beta = beta, n = n))
#       # Analyze those data
#       analyze_data(df)
#     },
#     future.seed = TRUE)
#     # Calculate the proportion of significant results using mean()
#     mean(out)
#   })
#   data.frame(study_design, power = pwr)
# }

## ----eval = FALSE-------------------------------------------------------------
# targets::tar_make()

## ----eval = FALSE-------------------------------------------------------------
# worcs::add_testthat()

## ----eval = FALSE-------------------------------------------------------------
# worcs::test_worcs()

## ----eval = FALSE-------------------------------------------------------------
# testthat::test_file("tests/testthat/test-generate_data.R")

## ----eval = FALSE-------------------------------------------------------------
# test_that("generate_data works", {
#   # Run generate_data()
#   df <- generate_data(.4, 100)
#   # It generates a `data.frame`
#   expect_s3_class(df, "data.frame")
#   # All columns are `numeric`
#   expect_true(all(sapply(df, inherits, what = "numeric")))
#   # The number of rows corresponds to `n`
#   expect_true(nrow(df) == 100)
#   # At high n, the regression coefficient approaches beta within tolerance
#   set.seed(1)
#   df <- generate_data(.4, 100000)
#   res <- lm(wellbeing ~ intrinsic_motivation + needs, data = df)
#   expect_equivalent(res$coefficients[2], .4, tolerance = .01)
# })

## ----eval = FALSE-------------------------------------------------------------
# test_that("generate_data generates a data.frame", {
#   # Run generate_data()
#   df <- generate_data(.4, 100)
#   # It generates a `data.frame`
#   expect_s3_class(df, "data.frame")
# })
# 
# test_that("generate_data returns all numeric columns", {
#   df <- generate_data(.4, 100)
#   expect_true(all(sapply(df, inherits, what = "numeric")))
# })

## ----eval = FALSE-------------------------------------------------------------
# # Add the appropriate GitHub action:
# worcs::github_action_testthat()

## ----eval = FALSE-------------------------------------------------------------
# renv::snapshot()

## ----eval = FALSE-------------------------------------------------------------
# worcs::git_update("add testthat")

## ----eval = FALSE-------------------------------------------------------------
# utils::browseURL(gsub(".git", "/actions", gert::git_remote_list()$url, fixed = TRUE))

