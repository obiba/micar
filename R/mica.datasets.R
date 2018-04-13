#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the datasets
#' 
#' @title Get the datasets
#' @param mica A Mica object
#' @param query The search query
#' @param select The fields to be extracted
#' @param sort The fields to sort by
#' @param from From item
#' @param limit Max number of items
#' @param locale The language for labels (default is "en")
#' @param df Return a data.frame (default is TRUE)
#' @export
mica.datasets <- function(mica, query="dataset()", 
                          select=list("acronym","name","studyTable","harmonizationTable"), sort=list("id"), 
                          from=0, limit=10000, locale="en", df=TRUE) {
  q <- .append.rql(query, "dataset", select, sort, from, limit, locale)
  res <- .get(mica, "datasets", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  .reportListMetrics(res)
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