#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the study populations. Note that the populations are not fetched directly, they are extracted from study search results.
#'
#' @title Get the study populations
#' @family studies functions
#' @param mica A Mica object
#' @param query The search query
#' @param select The fields to be extracted
#' @param sort The fields to sort by
#' @param from From item
#' @param limit Max number of items. Note that this limit applies to the studies that are fetched, the resulting count of populations will be higher.
#' @param locale The language for labels (default is "en")
#' @param df Return a data.frame (default is TRUE)
#' @examples
#' \dontrun{
#' m <- mica.login("https://mica-demo.obiba.org")
#' mica.study.populations(m, query="study(in(Mica_study.methods-design,cohort_study))")
#' mica.logout(m)
#' }
#' @export
mica.study.populations <- function(mica, query="study()",
                         select=list("populations.name","populations.description","populations.model"),
                         sort=list("id"),
                         from=0, limit=100, locale="en", df=TRUE) {
  q <- .append.rql(query, "study", select, sort, from, limit, locale)
  res <- .post(mica, "studies", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  .reportListMetrics(res)
  resultPropName <- ifelse(mica$needsLegacySupport, "obiba.mica.StudyResultDto.result", "studyResult")
  summaries <- res[["studyResultDto"]][[resultPropName]][["summaries"]]
  if (length(summaries)>0) {
    # get the count of populations
    populationsCount <- 0
    for (s in summaries) {
      populationsCount <- populationsCount + length(s$populationSummaries)
    }
    id <- rep(NA, populationsCount)
    name <- rep(NA, populationsCount)
    description <- rep(NA, populationsCount)
    studyId <- rep(NA, populationsCount)
    model <- list()
    idx <- 0
    for(i in 1:length(summaries)) {
      s <- summaries[[i]]
      if (!is.null(s$populationSummaries)) {
        for (pop in s$populationSummaries) {
          idx <- idx + 1
          id[idx] <- paste0(s[["id"]], ":", pop[["id"]])
          name[idx] <- .extractLabel(locale, pop[["name"]])
          description[idx] <- .extractLabel(locale, pop[["description"]])
          studyId[[idx]] <- s[["id"]]
          if (!is.null(pop[["content"]])) {
            ct <- .flatten(jsonlite::fromJSON(pop[["content"]], simplifyDataFrame = FALSE), locale)
            for (key in names(ct)) {
              if (!(key %in% names(model))) {
                d <- list()
                d[[key]] <- rep(NA, populationsCount)
                model <- append(model, d)
              }
              model[[key]][idx] <- ct[[key]]
            }
          }
        }
      }
    }
    df <- data.frame(id, name, description, studyId)
    if (all(is.na(df$name))) {
      df$name <- NULL
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
