% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.ViSigrid.r
\docType{methods}
\name{plot-ViSigrid-method}
\alias{plot,ViSigrid-method}
\alias{plot-ViSigrid-method}
\title{Method \code{plot-ViSigrid}}
\usage{
\S4method{plot}{ViSigrid}(x, scal.unit.tps = 10, unit.tps = "s",
  main = " ", ncharlabel = 30, size.main = 12, Fontsize.title = 11,
  Fontsize.label.Action = 11, Fontsize.label.Time = 11,
  Fontsize.label.color = 9, col.main = "black", col.grid = "grey",
  colgreenzone = "green", colblackzone = "black", alphainf = 0.8,
  alphasup = 1, alphaZones = 0.2, vp0h = 0.6, vp0w = 0.6, linA = 0.7,
  rcircle = 15, lwdline = 2, lwd.grid = 1, lty.grid = 1)
}
\arguments{
\item{x}{A \code{ViSigrid} object built using the \code{\link{visielse}} function.}

\item{scal.unit.tps}{Unity of time for the grey grid legend.}

\item{unit.tps}{Unit of time (s,min,..).}

\item{main}{Title.}

\item{ncharlabel}{Maximum number of plotted characters for labels of actions.}

\item{size.main}{Title size.}

\item{Fontsize.title}{Fontsize of the title.}

\item{Fontsize.label.Action}{Fontsize of labels of plotted actions.}

\item{Fontsize.label.Time}{Fontsize of the time axe.}

\item{Fontsize.label.color}{Fontsize of legends.}

\item{col.main}{Title color.}

\item{col.grid}{Color of the legend box.}

\item{colgreenzone}{Color of the green zones.}

\item{colblackzone}{Color of black zones.}

\item{alphainf}{Alpha of informers circles.}

\item{alphasup}{Alpha of supplementary times.}

\item{alphaZones}{Alpha of green and black zones.}

\item{vp0h}{Height of the main plot window, <1.}

\item{vp0w}{Width  of the main plot window, <1.}

\item{linA}{Height of the plotting area in each actions lines, < 1.}

\item{rcircle}{circle radius of informers circles.}

\item{lwdline}{line width of lines linking the 3 informers circles.}

\item{lwd.grid}{Lines width of the legend box.}

\item{lty.grid}{Lines type of the legend box.}
}
\description{
Method plot for ViSigrid object. This method provides a graphic
of raw data during experimental observations of the realization of a procedure
like a medical algorithm. It graphically presents an overview of individuals
and group actions usually acquired from timestamps during video recorded sessions.
}
\seealso{
\code{\linkS4class{ViSigrid}}, \code{\linkS4class{ViSibook}},
 \code{\link{visielse}}.
}

