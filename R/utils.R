# Helper functions

#' Pipe
#'
#' Import pipe operator from magrittr
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @noRd
#' @export
#' @param lhs,rhs specify what lhs and rhs are
NULL


#' ggsci pal discrete.
#'
#' @param ggsci_pal_name ggsci palette name.
#' @param pal_size an integer of how many colors needed.
#' @param ... other arguments passed to colorRampPalette or ggsci pal
#'   functions (e.g. \code{\link[ggsci]{pal_jco}}).
#' @returns a character vector of colors.
ggsci_pal_d <- function(ggsci_pal_name = "jco", pal_size = 5, ...) {
  # jco
  if (ggsci_pal_name == "jco") {
    if (pal_size > 10) {
      pal <- ggsci::pal_jco(...)(10)
      pal <- colorRampPalette(pal, ...)(pal_size)
    } else {
      pal <- ggsci::pal_jco(...)(pal_size)
    }
  } else if (ggsci_pal_name == "startrek") {
    # Star trek
    if (pal_size > 7) {
      pal <- ggsci::pal_startrek(...)(7)
      pal <- colorRampPalette(pal, ...)(pal_size)
    } else {
      pal <- ggsci::pal_startrek(...)(pal_size)
    }
  } else if (ggsci_pal_name == "aaas") {
    # aaas
    if (pal_size > 10) {
      pal <- ggsci::pal_aaas(...)(10)
      pal <- colorRampPalette(pal, ...)(pal_size)
    } else {
      pal <- ggsci::pal_aaas(...)(pal_size)
    }
  } else if (ggsci_pal_name == "npg") {
    # NPG
    if (pal_size > 10) {
      pal <- ggsci::pal_npg(...)(10)
      pal <- colorRampPalette(pal, ...)(pal_size)
    } else {
      pal <- ggsci::pal_npg(...)(pal_size)
    }
  } else {
    stop("pal not found")
  }
  pal
}
