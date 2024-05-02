#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
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
#' @family studies functions
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
#' mica.studies(m, query="variable(in(Mlstr_area.Lifestyle_behaviours,Drugs))",
#'   locale="en", from=0, limit=10)
#' mica.logout(m)
#' }
#' @export
mica.studies <- function(mica, query="study()",
                         select=list("acronym","name","objectives","model","populations.dataCollectionEvents.model.dataSources"),
                         sort=list("id"),
                         from=0, limit=100, locale="en", df=TRUE) {
  q <- .append.rql(query, "study", select, sort, from, limit, locale)
  res <- .post(mica, "studies", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  .reportListMetrics(res)
  summaries <- res[["studyResultDto"]][["studyResult"]][["summaries"]]
  if (length(summaries)>0) {
    id <- rep(NA, length(summaries))
    name <- rep(NA, length(summaries))
    acronym <- rep(NA, length(summaries))
    objectives <- rep(NA, length(summaries))
    design <- rep(NA, length(summaries))
    targetNumber <- rep(NA, length(summaries))
    dataSources <- list()
    model <- list()
    variables <- rep(NA, length(summaries))
    collectedDatasets <- rep(NA, length(summaries))
    collectedVariables <- rep(NA, length(summaries))
    harmonizedDatasets <- rep(NA, length(summaries))
    dataschemaVariables <- rep(NA, length(summaries))
    for(i in 1:length(summaries)) {
      s <- summaries[[i]]
      id[i] <- s[["id"]]
      name[i] <- .extractLabel(locale, s[["name"]])
      acronym[i] <- .extractLabel(locale, s[["acronym"]])
      objectives[i] <- .extractLabel(locale, s[["objectives"]])
      design[i] <- .nullToNA(s[["design"]])
      targetNumber[i] <- .nullToNA(s[["targetNumber"]][["number"]])
      if (!is.null(s[["dataSources"]])) {
        for (ds in s[["dataSources"]]) {
          key <- paste0("dataSources.",ds)
          if (!(key %in% names(dataSources))) {
            d <- list()
            d[[key]] <- rep(FALSE, length(summaries))
            dataSources <- append(dataSources, d)
          }
          dataSources[[key]][i] <- TRUE
        }
      }
      if (!is.null(s[["content"]])) {
        ct <- .flatten(jsonlite::fromJSON(s[["content"]], simplifyDataFrame = FALSE), locale)
        for (key in names(ct)) {
          if (!(key %in% names(model))) {
            d <- list()
            d[[key]] <- rep(NA, length(summaries))
            model <- append(model, d)
          }
          model[[key]][i] <- ct[[key]]
        }
      }
      counts <- s[["countStats"]]
      variables[i] <- .nullToNA(counts[["variables"]])
      collectedDatasets[i] <- .nullToNA(counts[["studyDatasets"]])
      collectedVariables[i] <- .nullToNA(counts[["studyVariables"]])
      harmonizedDatasets[i] <- .nullToNA(counts[["harmonizationDatasets"]])
      dataschemaVariables[i] <- .nullToNA(counts[["dataschemaVariables"]])
    }
    df <- data.frame(id, name, acronym, objectives,
                     variables, collectedDatasets, collectedVariables, harmonizedDatasets, dataschemaVariables)
    if (all(is.na(df$name))) {
      df$name <- NULL
    }
    if (all(is.na(df$acronym))) {
      df$acronym <- NULL
    }
    for (col in names(dataSources)) {
      df[[col]] <- dataSources[[col]]
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
