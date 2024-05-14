#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
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
#' @examples
#' \dontrun{
#' m <- mica.login("https://mica-demo.obiba.org")
#' mica.datasets(m, query="variable(in(Mlstr_area.Lifestyle_behaviours,Drugs))")
#' mica.logout(m)
#' }
#' @export
mica.datasets <- function(mica, query="dataset()",
                          select=list("*"), sort=list("id"),
                          from=0, limit=10000, locale="en", df=TRUE) {
  q <- .append.rql(query, "dataset", select, sort, from, limit, locale)
  res <- .post(mica, "datasets", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  .reportListMetrics(res)
  resultPropName <- ifelse(mica$needsLegacySupport, "obiba.mica.DatasetResultDto.result", "datasetResult")
  summaries <- res[["datasetResultDto"]][[resultPropName]][["datasets"]]
  if (length(summaries)>0) {
    id <- rep(NA, length(summaries))
    name <- rep(NA, length(summaries))
    acronym <- rep(NA, length(summaries))
    description <- rep(NA, length(summaries))
    variableType <- rep(NA, length(summaries))
    entityType <- rep(NA, length(summaries))
    studyId <- rep(NA, length(summaries))
    populationId <- rep(NA, length(summaries))
    dceId <- rep(NA, length(summaries))
    model <- list()
    variables <- rep(NA, length(summaries))
    networks <- rep(NA, length(summaries))
    hasStudyId<- FALSE
    for(i in 1:length(summaries)) {
      d <- summaries[[i]]
      id[i] <- d[["id"]]
      name[i] <- .extractLabel(locale, d[["name"]])
      acronym[i] <- .extractLabel(locale, d[["acronym"]])
      description[i] <- .extractLabel(locale, d[["description"]])
      variableType[i] <- d[["variableType"]]
      entityType[i] <- d[["entityType"]]
      harmonizedTypePropName <- ifelse(mica$needsLegacySupport, "obiba.mica.HarmonizedDatasetDto.type", "protocol")
      collectedTypePropName <- ifelse(mica$needsLegacySupport, "obiba.mica.CollectedDatasetDto.type", "collected")
      if (!is.null(d[[harmonizedTypePropName]]) && !is.null(d[[harmonizedTypePropName]][["harmonizationTable"]])) {
        hasStudyId <- TRUE
        studyId[i] <- d[[harmonizedTypePropName]][["harmonizationTable"]][["studyId"]]
        populationId[i] <- paste0(studyId[i], ":", d[[harmonizedTypePropName]][["harmonizationTable"]][["populationId"]])
      } else if (!is.null(d[[collectedTypePropName]]) && !is.null(d[[collectedTypePropName]][["studyTable"]])) {
        hasStudyId <- TRUE
        studyId[i] <- d[[collectedTypePropName]][["studyTable"]][["studyId"]]
        populationId[i] <- paste0(studyId[i], ":", d[[collectedTypePropName]][["studyTable"]][["populationId"]])
        dceId[i] <- d[[collectedTypePropName]][["studyTable"]][["dceId"]]
      }
      if (!is.null(d[["content"]])) {
        ct <- .flatten(jsonlite::fromJSON(d[["content"]], simplifyDataFrame = FALSE), locale)
        for (key in names(ct)) {
          if (!(key %in% names(model))) {
            l <- list()
            l[[key]] <- rep(NA, length(summaries))
            model <- append(model, l)
          }
          model[[key]][i] <- ct[[key]]
        }
      }
      countsPropName <- ifelse(mica$needsLegacySupport, "obiba.mica.CountStatsDto.datasetCountStats", "countStats")
      counts <- d[[countsPropName]]
      variables[i] <- .nullToNA(counts[["variables"]])
      networks[i] <- .nullToNA(counts[["networks"]])
    }
    df <- data.frame(id, name, acronym, description, variableType, entityType, studyId, populationId, dceId, variables, networks)
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
