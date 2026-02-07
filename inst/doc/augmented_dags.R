## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(theorytools)
library(dagitty)
library(tidySEM)

## ----eval = FALSE-------------------------------------------------------------
# library(dagitty)
# dagitty("dag {
#   X -> Y
#   Z -> X
#   Z -> Y
# }")

## ----eval = FALSE-------------------------------------------------------------
# library(dagitty)
# dagitty('dag {
#   X [exposure, pos="0,1"]
#   Y [outcome, pos="1,1"]
#   Z [unobserved, pos="1,0"]
#   X -> Y
#   Z -> X
#   Z -> Y
# }')

## ----eval = FALSE-------------------------------------------------------------
# library(tidySEM)
# g <- dagitty('dag {
#   X [label="Predictor", pos="0,0"]
#   Y [label="Outcome", pos="1,0"]
#   X -> Y [label="effect"]
# }')
# graph_sem(g, text_size = 2)

## ----eval = knitr::is_html_output(), echo = FALSE, out.width="60%"------------
library(tidySEM)
library(ggplot2)
g <- dagitty('dag {
  X [label="Predictor", pos="0,0"]
  Y [label="Outcome", pos="1,0"]
  X -> Y [label="effect"]
}')
p <- graph_sem(g, text_size = 4)
ggsave("dag_basic.png", p, device = "png", width = 4, height = 1)
knitr::include_graphics("dag_basic.png")

## -----------------------------------------------------------------------------
g <- dagitty('dag {
  X [distribution="rbinom(size = 2, prob = .5)"]
  Y [distribution="rnorm()"]
  X -> Y [form=".2*X"]
}')

## -----------------------------------------------------------------------------
g <- dagitty('dag {
  X [distribution="rbinom(size = 2, prob = .5)"]
  Y [distribution="rnorm()"]
  X -> Y [form=".2*X"]
}')

## ----augmentedDAG, echo=TRUE, eval = FALSE------------------------------------
# g <- dagitty('dag {
#   X [exposure,
#      pos="0,0",
#      label="Study Hours",
#      distribution="sample.int(n = 20, size = n, replace = TRUE)"]
#   Z [label="Stress Level",
#      pos=".5,1",
#      distribution="rexp()"]
#   Y [outcome,
#      pos="1,.2",
#      label="Exam Performance", distribution="rnorm()"]
#   X -> Y [label="direct", form="0.5+X"]
#   X -> Z
#   Z -> Y [label="indirect", form="2*Z"]
# }')
# graph_sem(g, text_size = 3)

## ----eval = knitr::is_html_output(), echo = FALSE, out.width="60%"------------
g <- dagitty('dag {
  X [exposure,
     pos="0,0",
     label="Study Hours",
     distribution="sample.int(n = 20, size = n, replace = TRUE)"]
  Z [label="Stress Level",
     pos=".5,1",
     distribution="rexp()"]
  Y [outcome,
     pos="1,.2",
     label="Exam Performance", distribution="rnorm()"]
  X -> Y [label="direct", form="-X^2+4*X"]
  X -> Z
  Z -> Y [label="indirect", form="2*Z"]
}')
graph_sem(g, text_size = 4) -> p
ggsave("dag_three.png", p, device = "png", width = 6, height = 3)
knitr::include_graphics("dag_three.png")

## ----nodeProperties, echo=TRUE------------------------------------------------
get_nodes(g)
get_edges(g)

## ----simulateData, echo=TRUE, eval=TRUE---------------------------------------
set.seed(1)
cat(simulate_data(g, run = FALSE), sep = "\n")

## -----------------------------------------------------------------------------
df <- simulate_data(g, run = TRUE)
ggplot2::ggplot(df, aes(x=X,y=Y,color=Z))+geom_point()

