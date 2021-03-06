% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph.R
\name{graph_chunks}
\alias{graph_chunks}
\title{graph_chunks}
\usage{
graph_chunks(module_filter = NULL, plot_gcam = FALSE,
  include_disabled = FALSE, quiet = TRUE)
}
\arguments{
\item{module_filter}{Optional name of module to filter by}

\item{plot_gcam}{Plot a node for GCAM (all XMLs feed to)?}

\item{include_disabled}{Plots nodes of disabled chunks?}

\item{quiet}{Suppress messages?}
}
\value{
Adjacency matrix showing chunk-to-chunk data flows
}
\description{
graph_chunks
}
