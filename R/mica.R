#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Open a connection with Mica.
#' 
#' @title Mica connection
#' 
#' @return A Mica object.
#' @param username User name in mica. Can be provided by "mica.username" option.
#' @param password User password in mica. Can be provided by "mica.password" option.
#' @param url Mica url or list of mica urls. Can be provided by "mica.url" option.
#' @param opts Curl options. Can be provided by "mica.opts" option.
#' @export
mica.open <- function(username=getOption("mica.username", "anonymous"), password=getOption("mica.password", "password"), url=getOption("mica.url"), opts=getOption("mica.opts", list())) {
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
#' @param locale The language for labels (default is "en")
#' @export
mica.studies <- function(mica, query=NULL, locale="en") {
  q <- paste0("locale(", locale, "),", query)
  res <- .get(mica, "studies", "_rql", query=list(query=q))
  summaries <- res[["studyResultDto"]][["obiba.mica.StudyResultDto.result"]][["summaries"]]
  id <- c()
  name <- c()
  acronym <- c()
  design <- c()
  targetNumber <- c()
  dataSources.questionnaires <- c()
  dataSources.physicalMeasures <- c()
  dataSources.biologicalSamples <- c()
  dataSources.others <- c()
  variables <- c()
  collectedDatasets <- c()
  collectedVariables <- c()
  harmonizedDatasets <- c()
  dataschemaVariables <- c()
  for(i in 1:length(summaries)) {
    s <- summaries[[i]]
    id <- append(id, s[["id"]])
    name <- append(name, .extractLabel(locale, s[["name"]]))
    acronym <- append(acronym, .extractLabel(locale, s[["acronym"]]))
    design <- append(design, .nullToNA(s[["design"]]))
    targetNumber <- append(targetNumber, .nullToNA(s[["targetNumber"]][["number"]]))
    dataSources.questionnaires <- append(dataSources.questionnaires, .nullToNA("questionnaires" %in% s[["dataSources"]]))
    dataSources.physicalMeasures <- append(dataSources.physicalMeasures, .nullToNA("physical_measures" %in% s[["dataSources"]]))
    dataSources.biologicalSamples <- append(dataSources.biologicalSamples, .nullToNA("biological_samples" %in% s[["dataSources"]]))
    dataSources.others <- append(dataSources.others, .nullToNA("others" %in% s[["dataSources"]]))
    counts <- s[["obiba.mica.CountStatsDto.studyCountStats"]]
    variables <- append(variables, .nullToNA(counts[["variables"]]))
    collectedDatasets <- append(collectedDatasets, .nullToNA(counts[["studyDatasets"]]))
    collectedVariables <- append(collectedVariables, .nullToNA(counts[["studyVariables"]]))
    harmonizedDatasets <- append(harmonizedDatasets, .nullToNA(counts[["harmonizationDatasets"]]))
    dataschemaVariables <- append(dataschemaVariables, .nullToNA(counts[["dataschemaVariables"]]))
  }
  data.frame(id, name, acronym, design, targetNumber, 
             dataSources.questionnaires, dataSources.physicalMeasures, dataSources.biologicalSamples, dataSources.others, 
             variables, collectedDatasets, collectedVariables, harmonizedDatasets, dataschemaVariables)
}

#' Get the variables
#' 
#' @title Get the variables
#' @param mica A Mica object
#' @param query The search query
#' @param locale The language for labels (default is "en")
#' @export
mica.variables <- function(mica, query=NULL, locale="en") {
  q <- paste0("locale(", locale, "),", query)
  res <- .get(mica, "variables", "_rql", query=list(query=q))
  summaries <- res[["variableResultDto"]][["obiba.mica.DatasetVariableResultDto.result"]][["summaries"]]
  id <- c()
  name <- c()
  dataset <- c()
  study <- c()
  variableType <- c()
  label <- c()
  for(i in 1:length(summaries)) {
    v <- summaries[[i]]
    id <- append(id, v[["id"]])
    name <- append(name, v[["name"]])
    dataset <- append(dataset, .extractLabel(locale, v[["datasetAcronym"]]))
    study <- append(study, .extractLabel(locale, v[["studyAcronym"]]))
    variableType <- append(variableType, v[["variableType"]])
    label <- append(label, .extractLabel(locale, v[["variableLabel"]]))
  }
  data.frame(id, name, dataset, study, variableType, label)
}


#' Get the taxonomies
#' 
#' @title Get the taxonomies
#' @param mica A Mica object
#' @param query The search query
#' @param locale The language for labels (default is NULL, in which case labels are not included in the result)
#' @param target What the taxonomy is about: variable (default), dataset, study, network
#' @param taxonomies Taxonomy names to subset. If NULL or empty al taxonomies are returned
#' @export
mica.taxonomies <- function(mica, query=NULL, locale=NULL, target="variable", taxonomies=NULL) {
  res <- .get(mica, "taxonomies", "_search", query=list(query=query, locale=locale, target=target))
  taxonomy <- c()
  taxonomy.title <- c()
  vocabulary <- c()
  vocabulary.title <- c()
  vocabulary.description <- c()
  term <- c()
  term.title <- c()
  term.description <- c()
  if (length(res)>0) {
    for (i in 1:length(res)) {
      taxo <- res[[i]][["taxonomy"]]
      if ((is.null(taxonomies) || length(taxonomies) == 0 || taxo$name %in% taxonomies) && length(taxo[["vocabularies"]])>0) {
        for (j in 1:length(taxo[["vocabularies"]])) {
          voc <- taxo[["vocabularies"]][[j]]
          if (length(voc[["terms"]])) {
            for (k in 1:length(voc[["terms"]])) {
              te <- voc[["terms"]][[k]]
              taxonomy <- append(taxonomy, taxo$name)
              vocabulary <- append(vocabulary, voc$name)
              term <- append(term, te$name)
              if (!is.null(locale)) {
                taxonomy.title <- append(taxonomy.title, .extractLabel2(locale, taxo$title))
                vocabulary.title <- append(vocabulary.title, .extractLabel2(locale, voc$title))
                vocabulary.description <- append(vocabulary.description, .extractLabel2(locale, voc$description))
                term.title <- append(term.title, .extractLabel2(locale, te$title))
                term.description <- append(term.description, .extractLabel2(locale, te$description))  
              }
            }
          }
        }
      }
    }
  }
  if (is.null(locale)) {
    data.frame(taxonomy, vocabulary, term)
  } else {
    data.frame(taxonomy, taxonomy.title, vocabulary, vocabulary.title, vocabulary.description, term, term.title, term.description)  
  }
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