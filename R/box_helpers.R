#' HTML output used for code examples
#'
#' @param label label of box
#' @param icon icon next to label
#' @param color color of background
#' @param border_color border color
#' @param code string of R code
#'
#' @returns HTML string to be used in a code chunk with "results='asis'"
#' @export
makeExampleBox <- function(label, icon, code) {
  glue::glue(
    '<div class="example-box {label} ">',
    '<b>{icon} {label}:</b>',
    '<pre><code>{code}</code></pre>',
    '</div>'
  )
}

makeBadBox <- function(code){
  makeExampleBox("Bad", "❌", code)
}

makeGoodBox <- function(code){
  makeExampleBox("Good", "⚠️", code)
}

makeBetterBox <- function(code){
  makeExampleBox("Better", "✅", code)
}
