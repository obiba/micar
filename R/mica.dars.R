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
#' @family data access requests functions
#' @param mica A Mica object
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.form(m)
#' mica.logout(m)
#' }
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

#' Get the data access requests main form.
#' 
#' @title Get the data access requests main form
#' @family data access requests functions
#' @param mica A Mica object
#' @param status Filter by status
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dars(m)
#' mica.logout(m)
#' }
#' @export
mica.dars <- function(mica, status=NULL, df=TRUE) {
  query <- list()
  if (!is.null(status)) {
    query <- list(status=status)
  }
  res <- .get(mica, "data-access-requests", query=query)
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    .darDTOToDF(res)
  } else {
    data.frame()
  }
}

#' Get the data access requests preliminary form.
#' 
#' @title Get the data access requests preliminary form
#' @family data access requests functions
#' @param mica A Mica object
#' @param status Filter by status
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.preliminaries(m)
#' mica.logout(m)
#' }
#' @export
mica.dar.preliminaries <- function(mica, status=NULL, df=TRUE) {
  query <- list()
  if (!is.null(status)) {
    query <- list(status=status)
  }
  dars <- .get(mica, "data-access-requests", query=query)
  # for each dar, get the corresponding preliminary
  res <- lapply(dars, function(dar) {
    .get(mica, "data-access-request", dar$id, "preliminary", dar$id)
  })
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    .darDTOToDF(res)
  } else {
    data.frame()
  }
}

#' Get a specific data access request main form.
#' 
#' @title Get a data access request
#' @family data access requests functions
#' @param mica A Mica object
#' @param id Data access request identifier
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar(m, "12345")
#' mica.logout(m)
#' }
#' @export
mica.dar <- function(mica, id) {
  res <- .get(mica, "data-access-request", id)
  if (!is.null(res$content)) {
    res$content <- jsonlite::fromJSON(res$content, simplifyDataFrame=FALSE)
  }
  res
}

#' Get the history of a specific data access request main form.
#' 
#' @title Get data access request history
#' @family data access requests functions
#' @param mica A Mica object
#' @param id Data access request identifier
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.history(m, "12345")
#' mica.logout(m)
#' }
#' @export
mica.dar.history <- function(mica, id, df=TRUE) {
  res <- .get(mica, "data-access-request", id)
  if (!df) {
    return(res$statusChangeHistory)
  }
  n <- length(res$statusChangeHistory)
  if (n > 0) {
    .darStatusDTOToDF(id, res$statusChangeHistory)
  } else {
    data.frame()
  }
}

#' Get a specific data access request preliminary form.
#' 
#' @title Get a data access request preliminary form
#' @family data access requests functions
#' @param mica A Mica object
#' @param id Data access request identifier
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.preliminary(m, "12345")
#' mica.logout(m)
#' }
#' @export
mica.dar.preliminary <- function(mica, id) {
  res <- .get(mica, "data-access-request", id, "preliminary", id)
  if (!is.null(res$content)) {
    res$content <- jsonlite::fromJSON(res$content, simplifyDataFrame=FALSE)
  }
  res
}

#' Get the history of a specific data access request preliminary form.
#' 
#' @title Get data access request history preliminary form
#' @family data access requests functions
#' @param mica A Mica object
#' @param id Data access request identifier
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.preliminary.history(m, "12345")
#' mica.logout(m)
#' }
#' @export
mica.dar.preliminary.history <- function(mica, id, df=TRUE) {
  res <- .get(mica, "data-access-request", id, "preliminary", id)
  if (!df) {
    return(res$statusChangeHistory)
  }
  n <- length(res$statusChangeHistory)
  if (n > 0) {
    .darStatusDTOToDF(id, res$statusChangeHistory)
  } else {
    data.frame()
  }
}


#' Get a data access requests agreement forms.
#' 
#' @title Get a data access requests agreement forms
#' @family data access requests functions
#' @param mica A Mica object
#' @param id Data access request identifier
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.agreements(m, '1234')
#' mica.logout(m)
#' }
#' @export
mica.dar.agreements <- function(mica, id, df=TRUE) {
  res <- .get(mica, "data-access-request", id, "agreements")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    .darDTOToDF(res)
  } else {
    data.frame()
  }
}

#' Get the actions history of a specific data access request.
#' 
#' @title Get data access request actions
#' @family data access requests functions
#' @param mica A Mica object
#' @param id Data access request identifier
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.actions(m, "12345")
#' mica.logout(m)
#' }
#' @export
mica.dar.actions <- function(mica, id, df=TRUE) {
  res <- .get(mica, "data-access-request", id)
  res <- res$actionLogHistory
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n>0) {
    darId <- rep(NA, n)
    action <- rep(NA, n)
    author <- rep(NA, n)
    changedOn <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      if (!is.null(item$reference)) {
        hasReference <- TRUE
      }
      darId[i] <- id
      action[i] <- item$action
      author[i] <- item$author
      changedOn[i] <- item$changedOn
    }
    data.frame(id=darId, action, author, changedOn)  
  } else {
    data.frame()
  }
  
}

#' Get the data access request amendment form.
#' 
#' @title Get the data access request amendment form
#' @family data access requests functions
#' @param mica A Mica object
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.amendment.form(m)
#' mica.logout(m)
#' }
#' @export
mica.dar.amendment.form <- function(mica) {
  res <- .get(mica, "config", "data-access-amendment-form")
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

#' Get the list of amendments of a specific data access request.
#' 
#' @title Get amendments
#' @family data access requests functions
#' @param mica A Mica object
#' @param pid Data access request identifier
#' @param status Filter by status
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.amendments(m, "12345")
#' mica.logout(m)
#' }
#' @export
mica.dar.amendments <- function(mica, pid, status=NULL, df=TRUE) {
  query <- list()
  if (!is.null(status)) {
    query <- list(status=status)
  }
  res <- .get(mica, "data-access-request", pid, "amendments", query=query)
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    .darDTOToDF(res)
  } else {
    data.frame()
  }
}

#' Get the history of the amendments of a specific data access request.
#' 
#' @title Get amendments history
#' @family data access requests functions
#' @param mica A Mica object
#' @param pid Data access request identifier
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.amendments.history(m, "12345")
#' mica.logout(m)
#' }
#' @export
mica.dar.amendments.history <- function(mica, pid, df=TRUE) {
  res <- .get(mica, "data-access-request", pid, "amendments", "_history")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    .darStatusDTOToDF(pid, res)
  } else {
    data.frame()
  }
}

#' Get a specific data access request amendment.
#' 
#' @title Get a amendment
#' @family data access requests functions
#' @param mica A Mica object
#' @param pid Data access request identifier
#' @param id Amendment identifier
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.amendment(m, "12345", "12345-1")
#' mica.logout(m)
#' }
#' @export
mica.dar.amendment <- function(mica, pid, id) {
  res <- .get(mica, "data-access-request", pid, "amendment", id)
  if (!is.null(res$content)) {
    res$content <- jsonlite::fromJSON(res$content, simplifyDataFrame=FALSE)
  }
  res
}

#' Get the history of a specific data access request amendment.
#' 
#' @title Get amendment history
#' @family data access requests functions
#' @param mica A Mica object
#' @param pid Data access request identifier
#' @param id Amendment identifier
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
#' mica.dar.amendment.history(m, "12345", "12345-1")
#' mica.logout(m)
#' }
#' @export
mica.dar.amendment.history <- function(mica, pid, id, df=TRUE) {
  res <- .get(mica, "data-access-request", pid, "amendment", id)
  if (!df) {
    return(res$statusChangeHistory)
  }
  n <- length(res$statusChangeHistory)
  if (n > 0) {
    .darStatusDTOToDF(id, res$statusChangeHistory)
  } else {
    data.frame()
  }
}

#' Make a list of DataAccessRequest DTOs a data frame (applies to DARs and their amendments)
#' @keywords internal
.darDTOToDF <- function(res) {
  n <- length(res)
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
}

#' Make a list of StatusChange DTOs a data frame (applies to DARs and their amendments)
#' @keywords internal
.darStatusDTOToDF <- function(darId, res) {
  n <- length(res)
  id <- rep(NA, n)
  reference <- rep(NA, n)
  from <- rep(NA, n)
  to <- rep(NA, n)
  author <- rep(NA, n)
  changedOn <- rep(NA, n)
  hasReference <- FALSE
  for (i in 1:n) {
    item <- res[[i]]
    if (!is.null(item$reference)) {
      hasReference <- TRUE
    }
    id[i] <- darId
    reference[i] <- .nullToNA(item$reference)
    from[i] <- item$from
    to[i] <- item$to
    author[i] <- item$author
    changedOn[i] <- item$changedOn
  }
  if (hasReference) {
    data.frame(id, reference, from, to, author, changedOn)  
  } else {
    data.frame(id, from, to, author, changedOn)
  }
}