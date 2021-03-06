% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cap_outlier_iqr.R
\name{cap_outlier_iqr}
\alias{cap_outlier_iqr}
\title{Cap outlier by IQR method}
\usage{
cap_outlier_iqr(data, cols_index, iqr_threshold = 1.5, zero_min = F)
}
\arguments{
\item{data}{Dataframe to cap outliers}

\item{cols_index}{Which columns to check for outliers}

\item{iqr_threshold}{Number of IQRs to define outliers. Default=1.5}

\item{zero_min}{Use 0 as the minimum value. Default is false}
}
\value{
data with outliers capped
}
\description{
Cap outlier by IQR method
}
\examples{
cap_outlier_IQR(data,c(1,3,4),iqr_threshold=2,zero_min=T)
}
