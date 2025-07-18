#' Create Vignette with webexercises Support
#'
#' This function wraps \code{rmarkdown::html_document} to configure
#' compilation to embed the default webexercises CSS and JavaScript files in
#' the resulting HTML.
#'
#' @details Call this function as the \code{output_format} argument
#'   for the \code{\link[rmarkdown]{render}} function when compiling
#'   HTML documents from RMarkdown source.
#'
#' @param ... Additional function arguments to pass to
#'   \code{\link[rmarkdown]{html_document}}.
#'
#' @return R Markdown output format to pass to 'render'.
#'
#' @seealso \code{\link[rmarkdown]{render}}, \code{\link[rmarkdown]{html_document}}
# @export
webex_vignette <- function (...){
  Args <- list(
    css = system.file("rmarkdown", "templates", "html_vignette",
                      "resources", "vignette.css", package = "rmarkdown")
  )
  if(requireNamespace("webexercises", quietly = TRUE)){
    Args$css <- c(Args$css, system.file("reports/default/webex.css", package = "webexercises"))
    Args$includes <- rmarkdown::includes(after_body = system.file("reports/default/webex.js", package = "webexercises"))
    Args$md_extensions <- "-smart"
  }
  do.call(rmarkdown::html_vignette, Args)
}

vignette_bookdown_if_available <- function(...){
  if(requireNamespace("bookdown", quietly = TRUE)){
    bookdown::html_document2(...)
  } else {
    rmarkdown::html_vignette(...)
  }
}
