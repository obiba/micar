#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Insert some RQL statements to perform the query. TODO parse RQL properly!
#' @keywords internal
.append.rql <- function(query, target, select, sort, from, limit, locale) {
  f <- paste0(.as.rql("fields(", select),")")
  s <- .as.rql("sort", sort)
  l <- paste0("limit(", format(from, scientific = F),",", format(limit, scientific = F), ")")
  statement <- paste(f,l,s, sep=",")
  # normalize
  q <- query
  if (is.null(q) || q == "") {
    q <- paste0(target, "()")
  }
  # hack: replace target call with statement
  if (grepl(paste0(target, "\\(\\)"), q)) {
    q <- gsub(paste0(target,"\\(\\)"), paste0(target, "(", statement, ")"), q)
  } else if (grepl(paste0(target, "\\("), q)) {
    q <- gsub(paste0(target,"\\("), paste0(target, "(", statement, ","), q)
  } else {
    q <- paste0(target, "(", statement, "),", q)
  }
  q <- paste0(q, ",locale(", locale, ")")
  message("query: ",q)
  q
}

#' Utility method to build RQL atomic statement.
#' @keywords internal
.as.rql <- function(name, args) {
  paste(name, "(", paste(args, collapse=","), ")", sep="")
}

#' Creates a new RQL node that represents an operation.
#' 
#' @title New RQL node
#' @param name The name of the operator
#' @param ... The arguments of the operation
#' @export
rqlNode <- function(name,...) {
  node <- list(name=name,args=list(...))
  class(node) <- "RqlNode"
  node
}

#' @export
toString.RqlNode <- function(x, ...) {
  args <- ""
  if (class(x$args) == "list") {
    for (item in x$args) {
      if (args == "") {
        args <- toString(item)
      } else {
        args <- paste(args, toString(item), sep=",")  
      }
    }
  } else if (class(x$args) == "RqlNode") {
    args <- toString(x$args)
  }
  paste0(x$name,"(",toString(args),")", sep = "")
}