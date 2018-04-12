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

#' Get the networks
#' 
#' @title Get the networks
#' @param mica A Mica object
#' @param query The search query
#' @param locale The language for labels (default is "en")
#' @param df Return a data.frame (default is TRUE)
#' @export
mica.networks <- function(mica, query="network(fields((acronym,name,studyIds)),sort(id),limit(0,100))", locale="en", df=TRUE) {
  q <- paste0("locale(", locale, "),", query)
  res <- .get(mica, "networks", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  summaries <- res[["networkResultDto"]][["obiba.mica.NetworkResultDto.result"]][["networks"]]
  if (length(summaries)>0) {
    id <- rep(NA, length(summaries))
    name <- rep(NA, length(summaries))
    acronym <- rep(NA, length(summaries))
    studies <- rep(NA, length(summaries))
    variables <- rep(NA, length(summaries))
    collectedDatasets <- rep(NA, length(summaries))
    collectedVariables <- rep(NA, length(summaries))
    harmonizedDatasets <- rep(NA, length(summaries))
    dataschemaVariables <- rep(NA, length(summaries))
    if (length(summaries)>0) {
      for(i in 1:length(summaries)) {
        n <- summaries[[i]]
        id[i] <- n[["id"]]
        name[i] <- .extractLabel(locale, n[["name"]])
        acronym[i] <- .extractLabel(locale, n[["acronym"]])
        counts <- n[["obiba.mica.CountStatsDto.networkCountStats"]]
        studies[i] <- .nullToNA(counts[["studies"]])
        variables[i] <- .nullToNA(counts[["variables"]])
        collectedDatasets[i] <- .nullToNA(counts[["studyDatasets"]])
        collectedVariables[i] <- .nullToNA(counts[["studyVariables"]])
        harmonizedDatasets[i] <- .nullToNA(counts[["harmonizationDatasets"]])
        dataschemaVariables[i] <- .nullToNA(counts[["dataschemaVariables"]])
      }
    }
    df <- data.frame(id, name, acronym,
               studies, variables, collectedDatasets, collectedVariables, harmonizedDatasets, dataschemaVariables)
    if (all(is.na(df$name))) {
      df$name <- NULL
    }
    if (all(is.na(df$acronym))) {
      df$acronym <- NULL
    }
    df
  } else {
    data.frame()
  }
}

#' Get the studies
#' 
#' @title Get the studies
#' @param mica A Mica object
#' @param query The search query
#' @param locale The language for labels (default is "en")
#' @param df Return a data.frame (default is TRUE)
#' @export
mica.studies <- function(mica, query="study(fields((acronym,name,model.methods.design,populations.dataCollectionEvents.model.dataSources,model.numberOfParticipants.participant)),sort(id),limit(0,100))", locale="en", df=TRUE) {
  q <- paste0("locale(", locale, "),", query)
  res <- .get(mica, "studies", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  summaries <- res[["studyResultDto"]][["obiba.mica.StudyResultDto.result"]][["summaries"]]
  if (length(summaries)>0) {
    id <- rep(NA, length(summaries))
    name <- rep(NA, length(summaries))
    acronym <- rep(NA, length(summaries))
    design <- rep(NA, length(summaries))
    targetNumber <- rep(NA, length(summaries))
    dataSources.questionnaires <- rep(NA, length(summaries))
    dataSources.physicalMeasures <- rep(NA, length(summaries))
    dataSources.biologicalSamples <- rep(NA, length(summaries))
    dataSources.others <- rep(NA, length(summaries))
    variables <- rep(NA, length(summaries))
    collectedDatasets <- rep(NA, length(summaries))
    collectedVariables <- rep(NA, length(summaries))
    harmonizedDatasets <- rep(NA, length(summaries))
    dataschemaVariables <- rep(NA, length(summaries))
    hasDataSources <- FALSE
    for(i in 1:length(summaries)) {
      s <- summaries[[i]]
      id[i] <- s[["id"]]
      name[i] <- .extractLabel(locale, s[["name"]])
      acronym[i] <- .extractLabel(locale, s[["acronym"]])
      design[i] <- .nullToNA(s[["design"]]) 
      targetNumber[i] <- .nullToNA(s[["targetNumber"]][["number"]])
      if (!is.null(s[["dataSources"]])) {
        hasDataSources <- TRUE
        dataSources.questionnaires[i] <- .nullToNA("questionnaires" %in% s[["dataSources"]])
        dataSources.physicalMeasures[i] <- .nullToNA("physical_measures" %in% s[["dataSources"]])
        dataSources.biologicalSamples[i] <- .nullToNA("biological_samples" %in% s[["dataSources"]])
        dataSources.others[i] <- .nullToNA("others" %in% s[["dataSources"]])
      }
      counts <- s[["obiba.mica.CountStatsDto.studyCountStats"]]
      variables[i] <- .nullToNA(counts[["variables"]])
      collectedDatasets[i] <- .nullToNA(counts[["studyDatasets"]])
      collectedVariables[i] <- .nullToNA(counts[["studyVariables"]])
      harmonizedDatasets[i] <- .nullToNA(counts[["harmonizationDatasets"]])
      dataschemaVariables[i] <- .nullToNA(counts[["dataschemaVariables"]])
    }
    df <- data.frame(id, name, acronym, design, targetNumber, 
               dataSources.questionnaires, dataSources.physicalMeasures, dataSources.biologicalSamples, dataSources.others, 
               variables, collectedDatasets, collectedVariables, harmonizedDatasets, dataschemaVariables)
    if (all(is.na(df$name))) {
      df$name <- NULL
    }
    if (all(is.na(df$acronym))) {
      df$acronym <- NULL
    }
    if (all(is.na(df$design))) {
      df$design <- NULL
    }
    if (all(is.na(df$targetNumber))) {
      df$targetNumber <- NULL
    }
    if (!hasDataSources) {
      df$dataSources.questionnaires <- NULL
      df$dataSources.physicalMeasures <- NULL
      df$dataSources.biologicalSamples <- NULL
      df$dataSources.others <- NULL
    }
    df
  } else {
    data.frame()
  }
}

#' Get the datasets
#' 
#' @title Get the datasets
#' @param mica A Mica object
#' @param query The search query
#' @param locale The language for labels (default is "en")
#' @param df Return a data.frame (default is TRUE)
#' @export
mica.datasets <- function(mica, query="dataset(fields((acronym,name,studyTable,harmonizationTable)),sort(id),limit(0,100))", locale="en", df=TRUE) {
  q <- paste0("locale(", locale, "),", query)
  res <- .get(mica, "datasets", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  summaries <- res[["datasetResultDto"]][["obiba.mica.DatasetResultDto.result"]][["datasets"]]
  if (length(summaries)>0) {
    id <- rep(NA, length(summaries))
    name <- rep(NA, length(summaries))
    acronym <- rep(NA, length(summaries))
    variableType <- rep(NA, length(summaries))
    entityType <- rep(NA, length(summaries))
    studyId <- rep(NA, length(summaries))
    variables <- rep(NA, length(summaries))
    networks <- rep(NA, length(summaries))
    hasStudyId<- FALSE
    for(i in 1:length(summaries)) {
      d <- summaries[[i]]
      id[i] <- d[["id"]]
      name[i] <- .extractLabel(locale, d[["name"]])
      acronym[i] <- .extractLabel(locale, d[["acronym"]])
      variableType[i] <- d[["variableType"]]
      entityType[i] <- d[["entityType"]]
      if (!is.null(d[["obiba.mica.HarmonizedDatasetDto.type"]]) && !is.null(d[["obiba.mica.HarmonizedDatasetDto.type"]][["harmonizationTable"]])) {
        hasStudyId <- TRUE
        studyId[i] <- d[["obiba.mica.HarmonizedDatasetDto.type"]][["harmonizationTable"]][["studyId"]]
      } else if (!is.null(d[["obiba.mica.CollectedDatasetDto.type"]]) && !is.null(d[["obiba.mica.CollectedDatasetDto.type"]][["studyTable"]])) {
        hasStudyId <- TRUE
        studyId[i] <- d[["obiba.mica.CollectedDatasetDto.type"]][["studyTable"]][["studyId"]]
      }
      counts <- d[["obiba.mica.CountStatsDto.datasetCountStats"]]
      variables[i] <- .nullToNA(counts[["variables"]])
      networks[i] <- .nullToNA(counts[["networks"]])
    }
    df <- data.frame(id, name, acronym, variableType, entityType, studyId, variables, networks)
    if (all(is.na(df$name))) {
      df$name <- NULL
    }
    if (all(is.na(df$acronym))) {
      df$acronym <- NULL
    }
    if (!hasStudyId) {
      df$studyId <- NULL
      df$networks <- NULL
    }
    df
  } else {
    data.frame()
  }
}

#' Get the variables
#' 
#' @title Get the variables
#' @param mica A Mica object
#' @param query The search query
#' @param locale The language for labels (default is "en")
#' @param df Return a data.frame (default is TRUE)
#' @export
mica.variables <- function(mica, query="variable(fields((attributes.label)),sort(id),limit(0,10000))", locale="en", df=TRUE) {
  q <- paste0("locale(", locale, "),", query)
  res <- .get(mica, "variables", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  summaries <- res[["variableResultDto"]][["obiba.mica.DatasetVariableResultDto.result"]][["summaries"]]
  if (length(summaries)>0) {
    id <- rep(NA, length(summaries))
    name <- rep(NA, length(summaries))
    datasetId <- rep(NA, length(summaries))
    studyId <- rep(NA, length(summaries))
    variableType <- rep(NA, length(summaries))
    label <- rep(NA, length(summaries))
    for(i in 1:length(summaries)) {
      v <- summaries[[i]]
      id[i] <- v[["id"]]
      name[i] <- v[["name"]]
      datasetId[i] <- v[["datasetId"]]
      studyId[i] <- v[["studyId"]]
      variableType[i] <- v[["variableType"]]
      label[i] <- .extractLabel(locale, v[["variableLabel"]])
    }
    df <- data.frame(id, name, variableType, label, datasetId, studyId)
    if (all(is.na(df$label))) {
      df$label <- NULL
    }
    df
  } else {
    data.frame()
  }
}


#' Get the taxonomies
#' 
#' @title Get the taxonomies
#' @param mica A Mica object
#' @param query The search query
#' @param locale The language for labels (default is NULL, in which case labels are not included in the result)
#' @param target What the taxonomy is about: variable (default), dataset, study, network
#' @param taxonomies Taxonomy names to subset. If NULL or empty al taxonomies are returned
#' @param df Return a data.frame (default is TRUE)
#' @export
mica.taxonomies <- function(mica, query=NULL, locale=NULL, target="variable", taxonomies=NULL, df=TRUE) {
  res <- .get(mica, "taxonomies", "_search", query=list(query=query, locale=locale, target=target))
  if (!df) {
    return(res)
  }
  if (length(res)>0) {
    taxonomy <- c()
    taxonomy.title <- c()
    vocabulary <- c()
    vocabulary.title <- c()
    vocabulary.description <- c()
    term <- c()
    term.title <- c()
    term.description <- c()
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
    if (is.null(locale)) {
      data.frame(taxonomy, vocabulary, term)
    } else {
      data.frame(taxonomy, taxonomy.title, vocabulary, vocabulary.title, vocabulary.description, term, term.title, term.description)  
    }
  } else {
    data.frame()
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