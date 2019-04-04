# Build a table of IDAT files from the data directory where they are saved


#' Get IDAT file list
#'
#' @details
#' This function search the directory base (possibly including
#' subdirectories depending on the argument recursive for “sample sheet”
#' files (see below). These files are identified solely on the base of
#' their filename given by the arguments pattern "_(Grn|Red)\\.idat$"
#' and ignore case.
#'
#' @param base A character scalar of base directory path which has
#'   IDAT files.
#' @return A character vector of IDAT file paths.
get_idat_file_list <- function(base) {
  if (!dir.exists(base)) {
    stop("Base directory does not exist")
  }
  idat <- list.files(
    base,
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE,
    pattern = "_(Grn|Red)\\.idat$"
  )
  idat
}


#' Create IDAT table
#'
#' Create IDAT table from base directory
#'
#' @param base A character scalar of base directory path which has
#'   IDAT files.
#' @param rda A character scalar of file path which saves the return
#'   value. Default is NULL, - do not save.
#' @return An \code{IdatTable} object.
#' @export
create_idat_table <- function(base, rda = NULL) {
  idat_files <- get_idat_file_list(base = base)
  # Get basename and create time columns
  df <- data.frame(
    idat_files = idat_files,
    ctime = file.info(idat_files)$ctime) %>%
    dplyr::mutate(Basename = stringr::str_remove(idat_files, "_(Grn|Red)\\.idat$")) %>%
    dplyr::group_by(Basename) %>%
    dplyr::summarise(ctime = mean(ctime))
  # Get slide and array columns
  sentrix_info <- as.data.frame(stringr::str_split_fixed(basename(df$Basename), "_", n = 2),
                                stringsAsFactors = FALSE)
  colnames(sentrix_info) <- c("Slide", "Array")
  sentrix_info$Basename <- df$Basename
  # Add version column. The earliest version is 1.
  df %>%
    dplyr::left_join(sentrix_info, by = "Basename") %>%
    dplyr::arrange(Basename, ctime) %>%
    dplyr::group_by(Slide, Array) %>%
    dplyr::mutate(Version = row_number()) %>%
    dplyr::arrange(Basename, Version) %>%
    dplyr::ungroup()
}

