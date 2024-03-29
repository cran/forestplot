% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forestplot_helpers.R
\name{fpShapesGp}
\alias{fpShapesGp}
\title{A function for graphical parameters of the shapes used in \code{forestplot()}}
\usage{
fpShapesGp(
  default = NULL,
  box = NULL,
  lines = NULL,
  vertices = NULL,
  summary = NULL,
  zero = NULL,
  axes = NULL,
  hrz_lines = NULL,
  vrtcl_lines = NULL,
  grid = NULL
)
}
\arguments{
\item{default}{A fallback \link[grid:gpar]{grid::gpar} for all unspecified attributes.
If set to NULL then it defaults to legacy parameters, including
the \code{col}, \code{lwd.xaxis}, \code{lwd.ci} and \code{lty.ci}
parameter of \code{fpColors}.}

\item{box}{The graphical parameters (\code{gpar}, \code{character}) of the box, circle
or point indicating the point estimate, i.e. the middle
of the confidence interval (may be a list of gpars). If provided
a string a \code{gpar} will be generated with \code{col}, and \code{fill} for
those arguments.}

\item{lines}{The graphical parameters (\code{gpar}, \code{character}) of the confidence lines
(may be a list of gpars). If provided a string a \code{gpar} will be generated
with \code{col} as the only arguments.}

\item{vertices}{The graphical parameters (\code{gpar}, \code{character}) of the vertices
(may be a list of gpars).
If \code{ci.vertices} is set to TRUE in \code{forestplot}
\code{vertices} inherits from \code{lines} all its parameters but lty that is set
to "solid" by default.}

\item{summary}{The graphical parameters (\code{gpar}, \code{character}) of the summary
(may be a list of gpars). If provided a string a \code{gpar} will be generated with
\code{col}, and \code{fill} for those arguments.}

\item{zero}{The graphical parameters (\code{gpar}) of the zero line
(may not be a list of gpars). If provided a string a \code{gpar} will be generated
with \code{col} as the only arguments.}

\item{axes}{The graphical parameters (\code{gpar}) of the x-axis at the bottom
(may not be a list of gpars).}

\item{hrz_lines}{The graphical parameters (\code{gpar}) of the horizontal lines
(may not be a list of gpars). If provided a string a \code{gpar} will be generated
with \code{col} as the only arguments.}

\item{vrtcl_lines}{The graphical parameters (\code{gpar}) of the vertical lines
(may not be a list of gpars). If provided a string a \code{gpar} will be generated
with \code{col} as the only arguments.}

\item{grid}{The graphical parameters (\code{gpar}) of the grid (vertical lines)
(may be a list of gpars). If provided a string a \code{gpar} will be generated
with \code{col} as the only arguments.}
}
\value{
list A list with the elements:
\itemize{
\item default: the gpar for default attributes
\item box: the gpar or list of gpars of the box/marker
\item lines: the gpar or list of gpars of the lines
\item vertices: the gpar or list of gpars of the vertices
\item summary: the gpar or list of gpars of the summary
\item zero: the gpar of the zero vertical line
\item axes: the gpar of the x-axis
\item hrz_lines: the gpar of the horizontal lines
\item grid: the gpar or list of gpars of the grid lines
}
}
\description{
This function encapsulates all the non-text elements that are used in the
\code{\link[=forestplot]{forestplot()}} function. As there are plenty of shapes
options this function gathers them all in one place.
}
\details{
This function obsoletes \code{\link[=fpColors]{fpColors()}}.

If some, but not all parameters of a shape (e.g. box) are specified in gpar()
such as setting lwd but not line color, the unspecified parameters default
to the ones specified in \code{default}, then, default to legacy parameters
of \code{forestplot} such as \code{col}.

Parameters \code{box}, \code{lines}, \code{vertices}, \code{summary} may be set as list
containing several gpars. The length of the list must either be equal to the number of bands
per label or to the number of bands multiplied by the number of labels, allowing specification
of different styles for different parts of the forest plot.

The parameter \code{grid} can either be a single gpar or a list of gpars with as many
elements as there are lines in the grid (as set by the \code{xticks} or \code{grid}
arguments of forestplot)

Parameters \code{zero}, \code{axes}, \code{hrz_lines} must either be NULL or gpar
but cannot be lists of gpars.
}
\examples{
ask <- par(ask = TRUE)

# An example of how fpShapesGp works

styles <- fpShapesGp(
  default = gpar(col = "pink", lwd = 2, lineend = "square", linejoin = "mitre"),
  grid = list(
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue")
  ),
  box = list(
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue")
  ),
  lines = gpar(lty = "dashed"),
  vertices = gpar(lwd = 5, col = "red")
)

forestplot(
  labeltext = c("Author1", "Author2", "Author3", "Author4"),
  grid = c(1, 3, 5),
  mean = 1:4, lower = 0:3, upper = 2:5,
  shapes_gp = styles
)

par(ask = ask)
}
\seealso{
Other forestplot functions: 
\code{\link{forestplot}()},
\code{\link{fpColors}()},
\code{\link{fpDrawNormalCI}()},
\code{\link{fpLegend}()},
\code{\link{fp_add_lines}()},
\code{\link{fp_decorate_graph}()},
\code{\link{fp_insert_row}()},
\code{\link{fp_set_style}()},
\code{\link{fp_set_zebra_style}()}
}
\author{
Andre GILLIBERT
}
\concept{forestplot functions}
