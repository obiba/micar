#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
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
#' @examples
#' \dontrun{
#' m <- mica.login("https://mica-demo.obiba.org")
#' mica.variables(m, query="variable(in(Mlstr_area.Lifestyle_behaviours,Drugs))")
#' mica.logout(m)
#' }
#' @export
mica.variables <- function(mica, query="variable()",
                           select=list("*"),
                           sort=list("id"),
                           from=0, limit=10000, locale="en", df=TRUE) {
  q <- .append.rql(query, "variable", select, sort, from, limit, locale)
  res <- .post(mica, "variables", "_rql", query=list(query=q))
  if (!df) {
    return(res)
  }
  .reportListMetrics(res)
  resultPropName <- ifelse(mica$needsLegacySupport, "obiba.mica.DatasetVariableResultDto.result", "variableResult")
  summaries <- res[["variableResultDto"]][[resultPropName]][["summaries"]]
  if (length(summaries)>0) {
    id <- rep(NA, length(summaries))
    name <- rep(NA, length(summaries))
    valueType <- rep(NA, length(summaries))
    nature <- rep(NA, length(summaries))
    datasetId <- rep(NA, length(summaries))
    studyId <- rep(NA, length(summaries))
    populationId <- rep(NA, length(summaries))
    dceId <- rep(NA, length(summaries))
    variableType <- rep(NA, length(summaries))
    mimeType <- rep(NA, length(summaries))
    unit <- rep(NA, length(summaries))
    referencedEntityType <- rep(NA, length(summaries))
    repeatable <- rep(NA, length(summaries))
    occurrenceGroup <- rep(NA, length(summaries))
    label <- rep(NA, length(summaries))
    description <- rep(NA, length(summaries))
    categories <- rep(NA, length(summaries))
    categories.missing <- rep(NA, length(summaries))
    categories.label <- rep(NA, length(summaries))
    annotations <- list()
    for(i in 1:length(summaries)) {
      v <- summaries[[i]]
      id[i] <- v[["id"]]
      name[i] <- v[["name"]]
      valueType[i] <- .nullToNA(v[["valueType"]])
      nature[i] <- .nullToNA(v[["nature"]])
      datasetId[i] <- v[["datasetId"]]
      studyId[i] <- v[["studyId"]]
      populationId[i] <- .nullToNA(v[["populationId"]])
      dceId[i] <- .nullToNA(v[["dceId"]])
      variableType[i] <- v[["variableType"]]
      mimeType[i] <- .nullToNA(v[["mimeType"]])
      unit[i] <- .nullToNA(v[["unit"]])
      referencedEntityType[i] <- .nullToNA(v[["referencedEntityType"]])
      repeatable[i] <- .nullToNA(v[["repeatable"]])
      occurrenceGroup[i] <- .nullToNA(v[["occurrenceGroup"]])
      label[i] <- .extractLabel(locale, v[["variableLabel"]])
      description[i] <- .extractLabel(locale, v[["description"]])
      if ("categories" %in% names(v)) {
        categories[i] <- paste(lapply(v$categories, function(cat) { cat$name }), collapse = "|")
        categories.missing[i] <- paste(lapply(v$categories, function(cat) { cat$missing }), collapse = "|")
        categories.label[i] <- paste(lapply(v$categories, function(cat) {
          labels <- cat$attributes[lapply(cat$attributes, function(attr) { attr$name }) == "label"]
          if (length(labels)>0) {
            .extractLabel(locale, labels[[1]]$values)
          } else {
            ""
          }
          }), collapse = "|")
      }
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
    df <- data.frame(id, name, valueType, nature, categories, categories.missing, categories.label,
                     variableType, label, description, datasetId, studyId, populationId, dceId,
                     mimeType, unit, referencedEntityType, repeatable, occurrenceGroup)
    for (key in c("label", "categories", "categories.missing", "categories.label", "populationId", "dceId", "valueType", "nature", "mimeType", "unit", "referencedEntityType", "repeatable", "occurrenceGroup")) {
      if (all(is.na(df[key]))) {
        df[key] <- NULL
      }
    }
    for (col in names(annotations)) {
      df[[col]] <- annotations[[col]]
    }
    df
  } else {
    data.frame()
  }
}
