#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Open a connection with Mica and returns a Mica object.
#' 
#' @title Open connection with Mica
#' 
#' @return A Mica object.
#' @param username User name in mica. Can be provided by "mica.username" option.
#' @param password User password in mica. Can be provided by "mica.password" option.
#' @param url Mica url or list of mica urls. Can be provided by "mica.url" option.
#' @param opts Curl options. Can be provided by "mica.opts" option.
#' @export
mica.login <- function(username=getOption("mica.username", "anonymous"), password=getOption("mica.password", "password"), url=getOption("mica.url"), opts=getOption("mica.opts", list())) {
  if (is.null(url)) stop("mica url is required", call.=FALSE)
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
  
  r <- GET(.url(mica, "config"), httr::add_headers(Authorization = mica$authorization), .verbose())
  mica$config <- .handleResponse(mica, r)
  
  mica
}

#' Close connection and release resources of Mica.
#' 
#' @title Close connection with Mica
#' @param mica A Mica object
#' @export
mica.logout <- function(mica) {
  if (getOption("verbose", FALSE)) {
    r <- DELETE(.url(mica, "auth", "session", "_current"), httr::verbose())
  } else {
    r <- DELETE(.url(mica, "auth", "session", "_current"))
  }
  mica$sid <- NULL
}

#' @export
print.mica <- function(x, ...) {
  cat("url:", x$url, "\n")
  cat("name:", x$name, "\n")
  cat("version:", x$version, "\n")
  cat("username:", x$username, "\n")
  cat("session:", x$sid, "\n")
}

#' Extract label for locale. If not found, fallback to undefined language label (if any).
#' @keywords internal
.extractLabel2 <- function(locale="en", labels=list(), localeKey="locale", valueKey="text") {
  .extractLabel(locale, labels, localeKey=localeKey, valueKey=valueKey)
}

#' Extract label for locale. If not found, fallback to undefined language label (if any).
#' @keywords internal
.extractLabel <- function(locale="en", labels=list(), localeKey="lang", valueKey="value") {
  if (is.null(labels) || length(labels) == 0) {
    return(NA)
  }
  label <- NA
  label.und <- NA
  for (i in 1:length(labels)) {
    l <- labels[[i]]
    if (l[[localeKey]] == locale) {
      label <- l[[valueKey]]
    }
    if (l[[localeKey]] == "und") {
      label.und <- l[[valueKey]]
    }
  }
  if (is.na(label)) {
    label.und
  } else {
    label
  }
}

#' @keywords internal
.nullToNA <- function(x) {
  if (is.null(x)) {
    NA
  } else {
    x
  }
}

#' Verbose flag
#' @import httr
#' @keywords internal
.verbose <- function() {
  verbose <- NULL
  if (getOption("verbose", FALSE)) {
    verbose <- httr::verbose()
  }
  verbose
}

#' Issues a request to mica for the specified resource
#' @import httr
#' @keywords internal
.get <- function(mica, ..., query=list()) {
  r <- GET(.url(mica, ...), query=query, .verbose())
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
  cookies <- httr::cookies(response)
  mica$sid <- .extractMicaSessionId(cookies)
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
