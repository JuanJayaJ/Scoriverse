#' Apply Link Transformation
#'
#' Converts predictions between the link and response scales.
#'
#' @param predictions A numeric vector of predictions.
#' @param link_function A string specifying the link function (e.g., "log", "logit").
#' @param inverse Logical; if TRUE, applies the inverse transformation.
#' @return A numeric vector with transformed predictions.
#' @export
#' @importFrom stats plogis qlogis
apply_link_transform <- function(predictions, link_function, inverse = TRUE) {
  if (is.null(link_function)) return(predictions)

  if (inverse) {
    if (link_function == "log") {
      return(exp(predictions))
    } else if (link_function == "logit") {
      return(plogis(predictions))
    } else {
      stop("Unsupported link function for inverse transformation.")
    }
  } else {
    if (link_function == "log") {
      return(log(predictions))
    } else if (link_function == "logit") {
      return(qlogis(predictions))
    } else {
      stop("Unsupported link function for transformation.")
    }
  }
}

