## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# dir.create("dunning_kruger")
# setwd("dunning_kruger")

## -----------------------------------------------------------------------------
definitions <- 
"
## Definitions

- **performance** as a test score
- **performance estimation** as the difference between the expected and
the actual test score
- **skill** as the ability to perform well on a given test
- **overconfidence** as the difference between self-assessed and actual skill
- **measurement error** as luck on a test
"

cat(definitions, file="definitions.md")

## ----results='hide'-----------------------------------------------------------
library(igraph, warn.conflicts=FALSE)

g <- graph_from_literal(
    skill -- overconfidence,
    skill -- performance,
    overconfidence -- overestimation,
    performance -- overestimation,
    "skill + error" -- "overconfidence - error",
    "skill + error" -- performance,
    "expected performance - performance" -- overestimation,
    "expected performance - performance" -- "overconfidence - error"
)

E(g)$relationship <- c(
 "negative association",
 "~",
 "~",
 "negative association",
 ":=",
 ":=",
 "negative association",
 "= (Theorem 1)"
)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# plot(
#   g,
#   vertex.size = 20,
#   vertex.color = "white",
#   edge.label = E(g)$relationship,
# )

## ----echo = FALSE, eval = TRUE, message=FALSE---------------------------------
invisible({
svg(filename = "dk.svg")
set.seed(1)
plot(
  g,
  vertex.size = 20,
  vertex.color = "white",
  edge.label = E(g)$relationship,
)
dev.off()
})
if(file.exists("dk.svg")){
  knitr::include_graphics("dk.svg")
} else {
  set.seed(1)
plot(
  g,
  vertex.size = 20,
  vertex.color = "white",
  edge.label = E(g)$relationship,
)
}


## -----------------------------------------------------------------------------
write_graph(
  g,
  "relationship_graph.txt",
  format = "graphml"
)

## -----------------------------------------------------------------------------
worcs::add_license_file(path = ".", license = "cc0")

## -----------------------------------------------------------------------------
theorytools::add_readme_fair_theory(title = "Dunning-Kruger Effect",
                                    path = ".")

## ----eval---------------------------------------------------------------------
theorytools::add_zenodo_json_theory(
  path = ".",
  title = "Dunning-Kruger Effect",
  keywords = c("Dunning–Kruger", "Overconfidence", "Judgment error", "Measurement error")
)

## -----------------------------------------------------------------------------
gert::git_init(path = ".")

## ----eval = FALSE-------------------------------------------------------------
# worcs::git_remote_create("dunning_kruger", private = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# worcs::git_remote_connect(".", remote_repo = "dunning_kruger")

## -----------------------------------------------------------------------------
worcs::git_update("First commit of my theory", repo = ".")

