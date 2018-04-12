#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

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
  if (length(res)>0 && "taxonomy" %in% names(res[[1]])) {
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
