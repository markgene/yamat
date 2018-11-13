# Download data from public archives, such as GEO.


#' Create \code{RGChannelSet} object from GSE accession.
#'
#' Download IDAT files if available and then create
#' \code{\link[minfi]{RGChannelSet}}.
#'
#' @param gse_acc A character scalar of a single GSE accession.
#' @param data_dir A character scalar of the directory stored IDAT files.
#' @return A \code{\link[minfi]{RGChannelSet}} object.
#' @export
get_gse <- function(gse_acc, data_dir) {
  if (missing(gse_acc))
    stop("Require gse_acc argument.")
  if (missing(data_dir))
    data_dir <- tempdir()
  else
    dir.create(data_dir, recursive = TRUE)
  x <- GEOquery::getGEO(gse_acc)
  df <- Biobase::pData(x[[1]])
  if (!.has_idat(df)) {
    stop("Query GSE does not have IDAT files.")
  }
  # Download Green
  tmp <- sapply(
    seq(nrow(df)),
    function(i) {
      url <- as.character(df$supplementary_file[i])
      grn_file <- paste0(df$geo_accession[i], "_Grn.idat.gz")
      grn_file <- file.path(data_dir, grn_file)
      download.file(url, destfile = grn_file)
      R.utils::gunzip(grn_file)
    }
  )
  # Download Red
  tmp <- sapply(
    seq(nrow(df)),
    function(i) {
      url <- as.character(df$supplementary_file.1[i])
      red_file <- paste0(df$geo_accession[i], "_Red.idat.gz")
      red_file <- file.path(data_dir, red_file)
      download.file(url, destfile = red_file)
      R.utils::gunzip(red_file)
    }
  )
  # Create
  df$Basename <- file.path(data_dir, df$geo_accession)
  minfi::read.metharray.exp(targets = df)
}


.has_idat <- function(df) {
  grn_flag <- FALSE
  red_flag <- FALSE
  if ("supplementary_file" %in% colnames(df)) {
    df$supplementary_file %>%
      as.character(.) %>%
      stringr::str_detect(., "idat\\.gz$") %>%
      all() -> grn_flag
  }
  if ("supplementary_file.1" %in% colnames(df)) {
    df$supplementary_file.1 %>%
      as.character(.) %>%
      stringr::str_detect(., "idat\\.gz$") %>%
      all() -> red_flag
  }
  all(grn_flag, red_flag)
}
