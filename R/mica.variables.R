#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the variables
#' 
#' @title Get the variables
#' @param mica A Mica object
#' @param query The search query
#' @param select The fields to be extracted
#' @param sort The fields to sort by
#' @param from From item
#' @param limit Max number of items
#' @param locale The language for labels (default is "en")
#' @param df Return a data.frame (default is TRUE)
#' @export
mica.variables <- function(mica, query="variable()",  
                           select=list("attributes","nature","valueType"), sort=list("id"), 
                           from=0, limit=10000, locale="en", df=TRUE) {
  q <- .append.rql(query, "variable", select, sort, from, limit, locale)
  res <- .post(mica, "variables", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  .reportListMetrics(res)
  summaries <- res[["variableResultDto"]][["obiba.mica.DatasetVariableResultDto.result"]][["summaries"]]
  if (length(summaries)>0) {
    id <- rep(NA, length(summaries))
    name <- rep(NA, length(summaries))
    valueType <- rep(NA, length(summaries))
    nature <- rep(NA, length(summaries))
    datasetId <- rep(NA, length(summaries))
    studyId <- rep(NA, length(summaries))
    variableType <- rep(NA, length(summaries))
    label <- rep(NA, length(summaries))
    annotations <- list()
    for(i in 1:length(summaries)) {
      v <- summaries[[i]]
      id[i] <- v[["id"]]
      name[i] <- v[["name"]]
      valueType[i] <- .nullToNA(v[["valueType"]])
      nature[i] <- .nullToNA(v[["nature"]])
      datasetId[i] <- v[["datasetId"]]
      studyId[i] <- v[["studyId"]]
      variableType[i] <- v[["variableType"]]
      label[i] <- .extractLabel(locale, v[["variableLabel"]])
      if ("annotations" %in% names(v)) {
        for (annot in  v[["annotations"]]) {
          key <- paste0(annot$taxonomy, ".", annot$vocabulary)
          if (!(key %in% names(annotations))) {
            a <- list()
            a[[key]] <- rep(NA, length(summaries))
            annotations <- append(annotations, a)
          }
          annotations[[key]][i] <- annot$value
        }
      }
    }
    df <- data.frame(id, name, valueType, nature, variableType, label, datasetId, studyId)
    if (all(is.na(df$label))) {
      df$label <- NULL
    }
    if (all(is.na(df$valueType))) {
      df$valueType <- NULL
    }
    if (all(is.na(df$nature))) {
      df$nature <- NULL
    }
    for (col in names(annotations)) {
      df[[col]] <- annotations[[col]]
    }
    df
  } else {
    data.frame()
  }
}
