#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Log in Mica.
#' 
#' @title Mica login
#' 
#' @return A mica object or a list of mica objects.
#' @param username User name in mica. Can be provided by "mica.username" option.
#' @param password User password in mica. Can be provided by "mica.password" option.
#' @param url Mica url or list of mica urls. Can be provided by "mica.url" option.
#' @param opts Curl options. Can be provided by "mica.opts" option.
#' @export
mica.login <- function(username=getOption("mica.username", "anonymous"), password=getOption("mica.password", "password"), url=getOption("mica.url"), opts=getOption("mica.opts", list())) {
  if (is.null(url)) stop("mica url is required", call.=FALSE)
  if(is.list(url)){
    lapply(url, function(u){mica.login(username, password, u, opts=opts)})
  } else {
    .mica.login(username, password, url, opts=opts)
  }
}

#' Create the mica object
#' @keywords internal
.mica.login <- function(username, password, url, opts=list()) {
  mica <- new.env(parent=globalenv())
  
  # Username
  mica$username <- username
  
  # Strip trailing slash
  mica$url <- sub("/$", "", url)
  
  # Domain name
  mica$name <- gsub("[:/].*", "", gsub("http[s]*://", "", mica$url))
  
  # Version default value
  mica$version <- NA
  
  # Authorization
  mica$authorization <- .authToken(username, password)
  
  class(mica) <- "mica"
  
  mica
}

#' @export
print.mica <- function(x, ...) {
  cat("url:", x$url, "\n")
  cat("name:", x$name, "\n")
  cat("version:", x$version, "\n")
  cat("username:", x$username, "\n")
}

#' Get the studies
#' 
#' @title Get the studies
#' @param mica A Mica object
#' @param query The search query
#' @export
mica.studies <- function(mica, query=NULL) {
  .get(mica, "studies", "_rql", query=list(query=query))
}

#' Get the variables
#' 
#' @title Get the variables
#' @param mica A Mica object
#' @param query The search query
#' @export
mica.variables <- function(mica, query=NULL) {
  .get(mica, "variables", "_rql", query=list(query=query))
}

#' Issues a request to mica for the specified resource
#' @import httr
#' @keywords internal
.get <- function(mica, ..., query=list()) {
  if (getOption("verbose", FALSE)) {
    r <- GET(.url(mica, ...), query=query, httr::add_headers(Authorization = mica$authorization), httr::verbose())
  } else {
    r <- GET(.url(mica, ...), query=query, httr::add_headers(Authorization = mica$authorization))
  }
  .handleResponse(mica, r)
}

#' Default request response handler.
#' @import httr
#' @keywords internal
.handleResponse <- function(mica, response) {
  headers <- httr::headers(response)
  if (is.null(mica$version) || is.na(mica$version)) {
    mica$version <- as.character(headers[tolower('X-Mica-Version')])
  }
  if (is.null(mica$sid)) {
    cookies <- httr::cookies(response)
    mica$sid <- .extractMicaSessionId(cookies)
  }
  content(response)
}

#' Extract micasid from cookie data frame.
#' @keywords internal
.extractMicaSessionId <- function(cookies) {
  if (nrow(cookies[cookies$name=="micasid",])>0) {
    sid <- cookies[cookies$name=="micasid",]$value
    if (!is.na(sid)) {
      return(sid)
    }
  }
  return(NULL)
}

#' Utility method to build urls. Concatenates all arguments and adds a '/' separator between each element
#' @keywords internal
.url <- function(mica, ...) {
  paste(mica$url, "ws", ..., sep="/")
}

#' Constructs the value for the Authorization header
#' @import jsonlite
#' @keywords internal
.authToken <- function(username, password) {
  paste("Basic", jsonlite::base64_enc(paste(username, password, sep=":")))
}