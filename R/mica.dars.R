#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the data access request form.
#' 
#' @title Get the data access request form
#' @param mica A Mica object
#' @export
mica.dar.form <- function(mica) {
  res <- .get(mica, "config", "data-access-form")
  if (!is.null(res$schema)) {
    res$schema <- jsonlite::fromJSON(res$schema, simplifyDataFrame=FALSE)
  }
  if (!is.null(res$definition)) {
    res$definition <- jsonlite::fromJSON(res$definition, simplifyDataFrame=FALSE)
  }
  if (!is.null(res$csvExportFormat)) {
    res$csvExportFormat <- jsonlite::fromJSON(res$csvExportFormat, simplifyDataFrame=FALSE)
  }
  res
}

#' Get the data access requests.
#' 
#' @title Get the data access requests
#' @param mica A Mica object
#' @param df Return a data.frame (default is TRUE)
#' @export
mica.dars <- function(mica, df=TRUE) {
  res <- .get(mica, "data-access-requests")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    id <- rep(NA, n)
    created <- rep(NA, n)
    lastUpdate <- rep(NA, n)
    applicant <- rep(NA, n)
    status <- rep(NA, n)
    title <- rep(NA, n)
    model <- list()
    for (i in 1:n) {
      item <- res[[i]]
      id[i] <- item$id
      created[i] <- item$timestamps$created
      lastUpdate[i] <- item$timestamps$lastUpdate
      applicant[i] <- item$applicant
      status[i] <- item$status
      title[i] <- .nullToNA(item$title)
      if (!is.null(item$content)) {
        ct <- .flatten(jsonlite::fromJSON(item$content, simplifyDataFrame = FALSE))
        for (key in names(ct)) {
          if (!grepl("\\.obibaFiles\\.", key) || grepl("\\.obibaFiles\\.fileName", key) || grepl("\\.obibaFiles\\.size", key)) {
            if (!(key %in% names(model))) {
              d <- list()
              d[[key]] <- rep(NA, n)
              model <- append(model, d)
            }
            model[[key]][i] <- ct[[key]]
          }
        }
      }
    }
    df <- data.frame(id, created, lastUpdate, applicant, status, title)
    for (col in names(model)) {
      if (!all(is.na(model[[col]]))) {
        df[[col]] <- model[[col]]
      }
    }
    df
  } else {
    data.frame()
  }
}

#' Get a specific data access request.
#' 
#' @title Get a data access request
#' @param mica A Mica object
#' @param id Data access request identifier
#' @export
mica.dar <- function(mica, id) {
  res <- .get(mica, "data-access-request", id)
  if (!is.null(res$content)) {
    res$content <- jsonlite::fromJSON(res$content, simplifyDataFrame=FALSE)
  }
  res
}