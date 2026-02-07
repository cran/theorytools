## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = all(c(
    require("tidySEM")
  )),
  comment = "#>"
)

## ----install-packages, eval=FALSE---------------------------------------------
# install.packages("theorytools")
# install.packages("dagitty")
# install.packages("tidySEM")

## ----load-libraries, results='hide'-------------------------------------------
library(theorytools)
library(dagitty)
library(tidySEM)
library(ggplot2)

## ----figmorris, echo = FALSE, out.width="70%", fig.cap="An interpretation of Morris' Tripartite Model of Emotion Regulation Development"----
lo <- get_layout(
  "",   "O",  "",   "", "",
  "",   "PP", "",   "ER",   "A",
  "",   "EC", "",   "",   "",
  "PC", "",   "CC", "",   "",
  rows = 4
)
edg <- data.frame(
  from = c("EC", "EC", "EC", "ER", "O", "PC", "PC",
           "PC", "PC", "PP", "PP", "PP", "O"),
  to =   c("A", "ER", "O", "A", "ER", "CC", "EC",
           "O", "PP", "EC", "ER", "O", "A"),
  curvature = c(NA, NA, 60, NA, NA, NA, NA, NA,
                NA, -60, NA, 60, NA))
p <- prepare_graph(edges = edg, layout = lo)
p$edges$connect_from = c("right", "right", "left",
                         "right", "right", "right",
                         "top", "top", "top", "left",
                         "right", "left", "right")
p$edges$connect_to = c("left", "left", "left", "left",
                       "left", "left", "left", "left",
                       "left", "left", "left", "left", "left")
p$edges$arrow <- "both"
p$edges$arrow[which(p$edges$from == "PC")] <- "last"
g <- plot(p) +
  geom_segment(aes(
    x = p$nodes$x[p$nodes$name == "CC"], 
    y = p$nodes$node_ymax[p$nodes$name == "CC"]+.2,
    yend = (p$nodes$node_ymax[p$nodes$name == "CC"]+2),
    linewidth = 4),
    arrow = arrow(length = unit(0.03, "npc"), ends = "both"))
ggsave("morris.png", g, device = "png", width = 6, height = 4)
knitr::include_graphics("morris.png")

## ----include=FALSE------------------------------------------------------------
tripartite <- dagitty('dag {
O
PP
EC
PC
CC
ER
A

PC -> CC
PC -> EC
PC -> PP
PC -> O

O -> ER [form="CC:O"];
PP -> ER [form="CC:PP"];
EC -> ER [form="CC:EC"];

ER -> A

CC -> ER [form="CC:O+CC:PP+CC:EC"];

PP -> O
EC -> O
PP -> EC
}')

## ----eval=knitr::is_html_output(), results='asis', echo=FALSE-----------------
theorytools:::quizz(
  "A DAG can have arrows that run left to right, arrows that run right to left, and bidirectional arrows." = FALSE,
  "An interaction effect of X1 and X2 on Y is represented in a DAG by drawing a directed arrow from X1 to Y, and from X2 to Y." = TRUE,
  "Information about functional form, like `[form=\"CC:O+CC:PP+CC:EC\"]`, is part of the DAG." = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# writeLines(tripartite, "tripartite_model.txt")

## ----eval = FALSE-------------------------------------------------------------
# worcs::check_git()
# worcs::check_github()

## ----eval = FALSE-------------------------------------------------------------
# create_fair_theory(
#   path = file.path("c:/theories", "tripartite_model"),
#   title = "Tripartite Model",
#   theory_file = "tripartite_model.txt",
#   remote_repo = "tripartite_model",
#   add_license = "cc0")

## ----eval = FALSE-------------------------------------------------------------
# worcs::git_release_publish(repo = file.path("c:/theories", "tripartite_model"))

## ----eval = FALSE-------------------------------------------------------------
# download_theory(
#   id = "https://doi.org/10.5281/zenodo.14921521",
#   path = "c:/theories/tripartite_downloaded")

## ----eval = FALSE-------------------------------------------------------------
# download_theory(
#   id = "https://github.com/cjvanlissa/tripartite_model.git",
#   path = "c:/theories/tripartite_clone")

## ----eval = FALSE-------------------------------------------------------------
# tripartite <- dagitty(paste(readLines("c:/theories/tripartite_downloaded/tripartite_model.txt"), collapse = "\n"))

## ----eval = knitr::is_html_output()-------------------------------------------
graph_sem(tripartite)

## -----------------------------------------------------------------------------
lo <- get_layout(
  "",   "O",  "",   "", "",
  "",   "PP", "",   "ER",   "A",
  "",   "EC", "",   "",   "",
  "PC", "",   "CC", "",   "",
  rows = 4
)
graph_sem(tripartite, layout = lo)

## ----eval=knitr::is_html_output(), results='asis', echo=FALSE-----------------
theorytools:::quizz(
  "Which of the following describes the property of X-interoperability of a FAIR theory?" = c(answer = "It can be reused for a specific operation.", "It can be reused by any person.", "It can be reused for any operation.", "It is licenced to be reused."),
  "If you are contributing to theory development, which platform provides infrastructure to coordinate collaborate with known others and strangers?" = c(answer = "GitHub", "Zenodo", "RStudio"))

## ----simulate-data------------------------------------------------------------
set.seed(1)
df_sim <- simulate_data(tripartite, n = 497)
head(df_sim)

## -----------------------------------------------------------------------------
tripartite_coef <- dagitty('dag {
O
PP
EC
PC
CC
ER
A

PC -> CC [beta=.4]
PC -> EC [beta=.2]
PC -> PP [beta=.2]
PC -> O [beta=0]

O -> ER [beta=.2]
PP -> ER [beta=0]
EC -> ER [beta=.2]

ER -> A [beta=.4]

CC -> ER [beta=.4];

PP -> O [beta=0]
EC -> O [beta=0]
PP -> EC  [beta=.2]
}')
set.seed(51)
df_sim <- simulateSEM(tripartite_coef, N = 497)

## ----adj-sets-----------------------------------------------------------------
adjustmentSets(tripartite, exposure="O", outcome="ER")

## ----eval=knitr::is_html_output(), results='asis', echo=FALSE-----------------
theorytools:::quizz(
  "When estimating the effect of Observation on Emotion Regulation, It is fine to control for both Child Characteristics and Parent Characteristics." = FALSE,
  "What is the smallest possible simple adjustment set for the effect of Child Characteristics on Adjustment?" = c(answer = "PC", "EC, O, PP", "EC, PC, PP"))

## -----------------------------------------------------------------------------
df_controls <- select_controls(tripartite, df_sim, exposure = "O", outcome = "ER")
model_bivariate <- lm(ER ~ O, df_controls)
model_causal <- lm(ER ~., df_controls)
summary(model_bivariate)
summary(model_causal)

## ----results='asis', echo=FALSE-----------------------------------------------
theorytools:::quizz(
  "What is the causal effect of Observation on Emotion Regulation?" = c(0.14, .01),
  "There is a significant causal effect of Observation on Emotion Regulation." = TRUE,
  "If the DAG is correct, then `model_bivariate` gives us an unbiased estimate of the effect of Observation on Emotion Regulation." = FALSE)

## -----------------------------------------------------------------------------
head(lsac)

## -----------------------------------------------------------------------------
operationalizations <- c(PP = "warmth", EC = "relationship_quality", CC = "temperament_negreact", ER = "emotion_regulation", A = "social_functioning", PC = "coping")

## ----eval = FALSE-------------------------------------------------------------
# # Impute missing data
# df_real <- VIM::kNN(lsac, numFun = median)
# names(df_real) <- names(operationalizations)[match(operationalizations, names(df_real))]

## ----echo = FALSE-------------------------------------------------------------
# saveRDS(df_real, "df_real.RData")
df_real <- readRDS("df_real.RData")

## -----------------------------------------------------------------------------
adjustmentSets(tripartite, exposure = "EC", outcome = "ER")

## -----------------------------------------------------------------------------
# Get all DAG-implied conditional independencies
cis <- impliedConditionalIndependencies(tripartite)
# Bonferroni-corrected confidence interval
bonferroni <- 1-(.05/length(cis))
# Conduct the tests
ci_tests <- localTests(tripartite, df_sim, type = "cis.loess", R = 1000, tests = cis, conf.level = bonferroni)
# Print result, with added significance asterisks
add_significance(ci_tests)

## -----------------------------------------------------------------------------
cis_real <- filter_conditional_independencies(cis, df_real)
bonferroni <- 1-(.05/length(cis_real))
# Conduct the tests
ci_tests <- localTests(tripartite, df_real, type = "cis.loess", R = 1000, tests = cis_real, conf.level = bonferroni)
# Print result, with added significance asterisks
add_significance(ci_tests)

## ----results='asis', echo=FALSE-----------------------------------------------
theorytools:::quizz(
  "We can use Parenting Practices because O is not in its adjustment set." = TRUE,
  "Which of these causal effects can we estimate using these data?" = c(answer = "All of these ", "PP -> A", "ER  -> PC", "CC -> A"),
  "The `ci_tests` give us reasons to doubt that `df_sim` is consistent with the DAG." = FALSE,
  "The `ci_tests` give us reasons to doubt that `df_real` is consistent with the DAG." = TRUE,
  "Consider the sample size of `df_real`. Could this be related to your answers in the previous two questions?" = c(answer = "Yes", "No")
)

