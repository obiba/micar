#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
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
#' @examples 
#' \dontrun{
#' # login using credentials from mica.username and mica.password options
#' m <- mica.login("https://mica-demo.obiba.org")
#' # login by providing credentials
#' m <- mica.login("administrator", "password", "https://mica-demo.obiba.org")
#' }
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
#' @examples 
#' \dontrun{
#' m <- mica.login("https://mica-demo.obiba.org")
#' mica.logout(m)
#' }
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

#' Display search result metrics
#' @keywords internal
.reportListMetrics <- function(results){
  if ("variableResultDto" %in% names(results)) {
    message("variables: ", results[["variableResultDto"]][["totalHits"]], "/", results[["variableResultDto"]][["totalCount"]])
  }
  if ("datasetResultDto" %in% names(results)) {
    message("datasets: ", results[["datasetResultDto"]][["totalHits"]], "/", results[["datasetResultDto"]][["totalCount"]])
  }
  if ("studyResultDto" %in% names(results)) {
    message("studies: ", results[["studyResultDto"]][["totalHits"]], "/", results[["studyResultDto"]][["totalCount"]])
  }
  if ("networkResultDto" %in% names(results)) {
    message("networks: ", results[["networkResultDto"]][["totalHits"]], "/", results[["networkResultDto"]][["totalCount"]])
  }
}

#' Flatten a list hierarchy
#' @keywords internal
.flatten <- function(content, locale="en") {
  rval <- list()
  flattenValue <- function(val) {
    ifelse(is.null(val), NA, ifelse(length(val)<2 && (is.logical(val) || is.numeric(val)), val, paste(val, collapse = "|")))
  }
  if (is.null(names(content))) {
    # a list, but not a named one
    for (i in 1:length(content)) {
      val <- content[[i]]
      if (!is.list(val)) {
        rval[[i]] <- flattenValue(val)
      } else {
        subct <- .flatten(val, locale)
        for (subn in names(subct)) {
          if (is.null(rval[[subn]])) {
            rval[[subn]] <- subct[[subn]]
          } else {
            rval[[subn]] <- paste0(rval[[subn]], "|", subct[[subn]])  
          }
        }
      }
    }
  } else {
    for (n in names(content)) {
      val <- content[[n]]
      if (!is.list(val)) {
        rval[[n]] <- flattenValue(val)
      } else {
        subct <- .flatten(val, locale)
        for (subn in names(subct)) {
          # localized value is attached to the parent node
          k <- ifelse(subn == locale, n,  paste0(n, ".", subn))
          rval[[k]] <- subct[[subn]]
        }
      }
    }  
  }
  rval
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

#' Issues a GET request to mica for the specified resource
#' @import httr
#' @keywords internal
.get <- function(mica, ..., query=list()) {
  r <- GET(.url(mica, ...), query=query, .verbose())
  .handleResponse(mica, r)
}

#' Issues a POST form request to mica for the specified resource
#' @import httr
#' @keywords internal
.post <- function(mica, ..., query=list()) {
  r <- POST(.url(mica, ...), body=query, encode=c("form"), .verbose())
  .handleResponse(mica, r)
}

#' Default request response handler.
#' @import httr
#' @keywords internal
.handleResponse <- function(mica, response) {
  if (is.null(mica$version) || is.na(mica$version)) {
    headers <- httr::headers(response)
    mica$version <- as.character(headers[tolower('X-Mica-Version')])
  }
  content <- content(response)
  if (response$status>=300) {
    if ("error" %in% names(content)) {
      if ("message" %in% names(content)) {
        stop(content$message, call.=FALSE)
      } else {
        stop(content$error, call.=FALSE)  
      }
    } else {
      stop(response$status, call.=FALSE)
    }
  }
  cookies <- httr::cookies(response)
  mica$sid <- .extractMicaSessionId(cookies)
  content
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
