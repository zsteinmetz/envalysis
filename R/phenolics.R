#' @family calibration
#' 
#' @title Degradation of phenolic compounds by Steinmetz et al. (2019)
#' 
#' @description
#' The sample data is stored in a list. It consists of two \code{data.frame}s: a
#' sequence table (\code{seq}) and a sample table (\code{samples}). 
#' 
#' The sequence table contains gas-chromatography/mass spectrometry measurement 
#' data of two phenolic compounds, these are tyrosol and vanillin. Besides the
#' samples, standard mixtures and extraction blanks were acquired in three
#' separate analysis batches. Each measurement resulted in an integrated peak
#' area.
#' 
#' The sample table describes the samples' origin from a 29-day degradation
#' experiment, in which the phenolic compounds were either degraded in the dark
#' by the native soil microbial community or photooxidized under UV irradiation.
#' The samples were processed in threefold replication. Their weight [g], the
#' volume [mL] of extract solution, and the dilution factor were recorded.
#' 
#' @format
#' A list containing two data.frames.
#' 
#' @author
#' Zacharias Steinmetz
#' 
#' @references 
#' Steinmetz, Z., Kurtz, M.P., Zubrod, J.P., Meyer, A.H., Elsner, M., Schaumann,
#' G.E. (2019) Biodegradation and photooxidation of phenolic compounds in soil—A
#' compound-specific stable isotope approach. \emph{Chemosphere} \bold{230},
#' 210-218. DOI: \doi{10.1016/j.chemosphere.2019.05.030}.
#' 
#' @docType data
#' @keywords data
#' @name phenolics
NULL
