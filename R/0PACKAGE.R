
#' @keywords internal 
#' 
#' @import cli
#' 
#' @import stats
#' 
'_PACKAGE'


#' @importFrom utils citation
.onAttach <- function(libname, pkgname) {
  
  # .onAttach(libname = 'lib', pkgname = 'groupedHyperframe') # nah..
  
  'maxEff' |>
    citation() |>
    format(style = 'text') |> # utils:::format.citation
    col_green() |>
    style_bold() |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
}


