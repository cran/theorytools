## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = isTRUE(Sys.info()["user"] == 'vanlissa'),
  comment = "#>"
)
in_packagedown <- tryCatch({pkgdown::in_pkgdown()}, error = function(e){ FALSE })

## ----setup--------------------------------------------------------------------
library(theorytools)

## ----include = in_packagedown, eval = TRUE, echo = FALSE, fig.align='center', out.width = "70%", fig.cap="Conceptual workflow for this task."----
knitr::include_graphics("https://github.com/OpenScienceMOOC/Module-5-Open-Research-Software-and-Open-Source/blob/master/content_development/images/Task2.png?raw=true")

## ----eval = FALSE-------------------------------------------------------------
# project_path <- file.path("c:/theories", "empirical_cycle")
# dir.create(project_path)

## ----echo = FALSE-------------------------------------------------------------
project_path <- file.path(tempdir(), "empirical_cycle")
dir.create(project_path)

## -----------------------------------------------------------------------------
writeLines(
  c("*Phase 1:* 'Observation': collection and grouping of empirical materials;
    (tentative) formation of hypotheses.",
    "*Phase 2:* 'Induction': formulation of hypotheses.", 
    "*Phase 3:* 'Deduction': derivation of specific consequences
    from the hypotheses, in the form of testable predictions.",
    "*Phase 4:* 'Testing': of the hypotheses against new empirical materials,
    by way of checking whether or not the predictions are fulfilled.",
    "*Phase 5:* 'Evaluation': of the outcome of the testing procedure
    with respect to the hypotheses or theories stated, as well as
    with a view to subsequent, continued or related, investigations."
), file.path(project_path, "theory.txt"))

## -----------------------------------------------------------------------------
theory <- 
"digraph {

  observation;
  induction;
  deduction;
  test;
  evaluation;
  
  observation -> induction;
  induction -> deduction;
  deduction -> test;
  test -> evaluation;
  evaluation -> observation;
  
}"

## -----------------------------------------------------------------------------
cat(theory, file = file.path(project_path, "empirical_cycle.dot"), sep = "\n")

## -----------------------------------------------------------------------------
worcs::add_license_file(path = project_path, license = "cc0")

## -----------------------------------------------------------------------------
theorytools::add_readme_fair_theory(title = "The Empirical Cycle",
                                    path = project_path)

## -----------------------------------------------------------------------------
add_zenodo_json_theory(
  path = project_path,
  title = "The Empirical Cycle",
  keywords = c("philosophy of science", "methodology")
)

## -----------------------------------------------------------------------------
worcs::check_git()

## -----------------------------------------------------------------------------
gert::git_init(path = project_path)

## -----------------------------------------------------------------------------
worcs::check_github()

## ----eval = FALSE-------------------------------------------------------------
# worcs::git_remote_create("empirical_cycle", private = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# worcs::git_remote_connect(project_path, remote_repo = "empirical_cycle")

## -----------------------------------------------------------------------------
worcs::git_update("First commit of my theory", repo = project_path)

## ----include = in_packagedown, eval = TRUE, echo = FALSE, out.width = "70%", fig.cap="Front Page of a 'GitHub' Repository"----
knitr::include_graphics("https://github.com/cjvanlissa/theorytools/blob/master/docs/images/github.png?raw=true")

## ----include = in_packagedown, eval = TRUE, echo = FALSE, fig.align='center', out.width = "70%", fig.cap="Sign up for 'Zenodo'"----
knitr::include_graphics("https://github.com/OpenScienceMOOC/Module-5-Open-Research-Software-and-Open-Source/blob/master/content_development/images/zenodo.png?raw=true")

## ----include = in_packagedown, eval = TRUE, echo = FALSE, fig.align='center', out.width = "70%", fig.cap="Authorize  to connect with 'GitHub'"----
knitr::include_graphics("https://github.com/OpenScienceMOOC/Module-5-Open-Research-Software-and-Open-Source/blob/master/content_development/images/_github.png?raw=true")

## ----include = in_packagedown, eval = TRUE, echo = FALSE, fig.align='center', out.width = "70%", fig.cap="Enable individual 'GitHub' repositories to be archived in 'Zenodo'"----
knitr::include_graphics("https://github.com/OpenScienceMOOC/Module-5-Open-Research-Software-and-Open-Source/blob/master/content_development/images/enabled_repos.png?raw=true")

## ----include = in_packagedown, eval = TRUE, echo = FALSE, fig.align='center', out.width = "70%", fig.cap="Check that webhooks are enabled for your 'GitHub' repository."----
knitr::include_graphics("https://github.com/OpenScienceMOOC/Module-5-Open-Research-Software-and-Open-Source/blob/master/content_development/images/webhooks.png?raw=true")

## ----eval = FALSE-------------------------------------------------------------
# worcs::git_release_publish(repo = project_path)

## ----eval = FALSE-------------------------------------------------------------
# worcs::git_release_publish(repo = project_path,
#                            tag_name = "0.2.0",
#                            release_name = "0.2.0")

## ----include = in_packagedown, eval = TRUE, echo = FALSE, fig.align='center', out.width = "70%", fig.cap="Check the new release has been uploaded."----
knitr::include_graphics("https://github.com/OpenScienceMOOC/Module-5-Open-Research-Software-and-Open-Source/blob/master/content_development/images/upload_release.png?raw=true")

## ----include = in_packagedown, eval = TRUE, echo = FALSE, fig.align='center', out.width = "70%", fig.cap="Click the orange Edit button."----
knitr::include_graphics("https://github.com/cjvanlissa/theorytools/blob/master/docs/images/zenodo_edit.png?raw=true")

## ----eval = FALSE-------------------------------------------------------------
# create_fair_theory(
#   path = file.path("c:/theories", "empirical_cycle"),
#   title = "The Empirical Cycle, Again",
#   theory_file = "theory.txt",
#   remote_repo = "empirical_cycle2",
#   add_license = "cc0")

