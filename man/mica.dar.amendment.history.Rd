% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mica.dars.R
\name{mica.dar.amendment.history}
\alias{mica.dar.amendment.history}
\title{Get amendment history}
\usage{
mica.dar.amendment.history(mica, pid, id, df = TRUE)
}
\arguments{
\item{mica}{A Mica object}

\item{pid}{Data access request identifier}

\item{id}{Amendment identifier}

\item{df}{Return a data.frame (default is TRUE)}
}
\description{
Get the history of a specific data access request amendment.
}
\examples{
\dontrun{
m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
mica.dar.amendment.history(m, "12345", "12345-1")
mica.logout(m)
}
}
\seealso{
Other data access requests functions: 
\code{\link{mica.dar.actions}()},
\code{\link{mica.dar.agreements}()},
\code{\link{mica.dar.amendment.form}()},
\code{\link{mica.dar.amendments.history}()},
\code{\link{mica.dar.amendments}()},
\code{\link{mica.dar.amendment}()},
\code{\link{mica.dar.form}()},
\code{\link{mica.dar.history}()},
\code{\link{mica.dar.preliminaries}()},
\code{\link{mica.dar.preliminary.history}()},
\code{\link{mica.dar.preliminary}()},
\code{\link{mica.dars}()},
\code{\link{mica.dar}()}
}
\concept{data access requests functions}
