#' Naughty URL manipulation
#'
#' @name naughtyurl
#'
#' @docType package
#' @useDynLib naughtyurl
NULL

#' @export
`[.naughty` <- function(z,i,j) .Call("vmatrix", z, i, j, PACKAGE='naughtyurl')

#' @export
naughty <- function(x) UseMethod("naughty")

#' @export
naughty.default <- function(x) stop("Not a character")

#' @export
naughty.character <- function(z) invisible(.Call("do_naughty", z, PACKAGE='naughtyurl'))

#' @export
unnaughty <- function(x) UseMethod("unnaughty")

#' @export
unnaughty.default <- function(x) stop("Not a naughty object")

#' @export
unnaughty.naughty <- function(z) invisible(.Call("do_unnaughty", z, PACKAGE='naughtyurl'))
