% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_generates_line_transect_data_set.R
\name{GenerateDataset}
\alias{GenerateDataset}
\title{GenerateDataset. Generates a random lineal transect data set}
\usage{
GenerateDataset(num.obs)
}
\arguments{
\item{num.obs}{Number of observations desired (numeric).}
}
\value{
Returns a dataset with the columns: subject, track, distance, transectID and num.obs observations.
}
\description{
This function returns a random dataset of distances measured over lineal transets. It uses parameters fixed within. The only parameter is num.obs which is a numeric and corresponds to the number of observations of the dataset.
}
\author{
Edgar Federico Rivadeneira, email = federivadeira@gmail.com
}
