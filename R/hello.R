#' Naughty URL manipulation
#'
#' @name naughtyurl
#'
#' @docType package
#' @useDynLib naughtyurl
NULL

#' @export
`[.naughty` <- function(x,i=NULL,j=NULL) .Call("vmatrix", x, i, j, PACKAGE='naughtyurl')
#' @export
`[<-.naughty` <- function(x,i=NULL,j=NULL, value) .Call("vmatrix_set", x, i, j, value, PACKAGE='naughtyurl')

#' @export
url_splice <- function(x, scheme, user, password, host, port, path, query, fragment){
  cols = integer(0)
  v = list()

  if(!missing(scheme))   {cols <- c(cols, 0x10L); v[['scheme']]<- scheme}
  if(!missing(user))     {cols <- c(cols, 0x11L); v[['user']]<- user}
  if(!missing(password)) {cols <- c(cols, 0x12L); v[['password']]<- password}
  if(!missing(host))     {cols <- c(cols, 0x13L); v[['host']]<- host}
  if(!missing(port))     {cols <- c(cols, 0x14L); v[['port']]<- port}
  if(!missing(path))     {cols <- c(cols, 0x15L); v[['path']]<- path}
  if(!missing(query))    {cols <- c(cols, 0x16L); v[['query']]<- query}
  if(!missing(fragment)) {cols <- c(cols, 0x17L); v[['fragment']]<- fragment}

  if(length(cols) == 0) return(x)

  .Call("do_splice", x, cols, v, PACKAGE='naughtyurl')
}

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
