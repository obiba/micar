#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the studies
#' 
#' @title Get the studies
#' @param mica A Mica object
#' @param query The search query
#' @param select The fields to be extracted
#' @param sort The fields to sort by
#' @param from From item
#' @param limit Max number of items
#' @param locale The language for labels (default is "en")
#' @param df Return a data.frame (default is TRUE)
#' @export
mica.studies <- function(mica, query="study()",
                         select=list("acronym","name","model.methods.design","populations.dataCollectionEvents.model.dataSources","model.numberOfParticipants.participant"), 
                         sort=list("id"), 
                         from=0, limit=100, locale="en", df=TRUE) {
  q <- .append.rql(query, "study", select, sort, from, limit, locale)
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
