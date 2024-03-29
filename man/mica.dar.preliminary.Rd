% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mica.dars.R
\name{mica.dar.preliminary}
\alias{mica.dar.preliminary}
\title{Get a data access request preliminary form}
\usage{
mica.dar.preliminary(mica, id)
}
\arguments{
\item{mica}{A Mica object}

\item{id}{Data access request identifier}
}
\description{
Get a specific data access request preliminary form.
}
\examples{
\dontrun{
m <- mica.login("someuser", "somepassword", "https://mica-demo.obiba.org")
mica.dar.preliminary(m, "12345")
mica.logout(m)
}
}
\seealso{
Other data access requests functions: 
\code{\link{mica.dar.actions}()},
\code{\link{mica.dar.agreements}()},
\code{\link{mica.dar.amendment.form}()},
\code{\link{mica.dar.amendment.history}()},
\code{\link{mica.dar.amendments.history}()},
\code{\link{mica.dar.amendments}()},
\code{\link{mica.dar.amendment}()},
\code{\link{mica.dar.form}()},
\code{\link{mica.dar.history}()},
\code{\link{mica.dar.preliminaries}()},
\code{\link{mica.dar.preliminary.history}()},
\code{\link{mica.dars}()},
\code{\link{mica.dar}()}
}
\concept{data access requests functions}
