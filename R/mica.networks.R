#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the networks
#' 
#' @title Get the networks
#' @param mica A Mica object
#' @param query The search query
#' @param select The fields to be extracted
#' @param sort The fields to sort by
#' @param from From item
#' @param limit Max number of items
#' @param locale The language for labels (default is "en")
#' @param df Return a data.frame (default is TRUE)
#' @export
mica.networks <- function(mica, query="network()", 
                          select=list("acronym", "name", "studyIds"), sort=list("id"), 
                          from=0, limit=100, locale="en", df=TRUE) {
  q <- .append.rql(query, "network", select, sort, from, limit, locale)
  res <- .get(mica, "networks", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  .reportListMetrics(res)
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