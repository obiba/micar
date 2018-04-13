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
                           select=list("attributes.label"), sort=list("id"), 
                           from=0, limit=10000, locale="en", df=TRUE) {
  q <- .append.rql(query, "variable", select, sort, from, limit, locale)
  res <- .get(mica, "variables", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  .reportListMetrics(res)
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
