#' @title S4 definition for flora-class objects
#'
#' @description
#'
#' flora-class objects store hierarchical key information for the floRa package
#'
#' @slot name      Name of the flora
#' @slot key       Hierarchical key for the flora. See details.
#' @slot species   List of species included in the flora. See details.
#' @slot state     US state(s) that flora is valid for.
#' @slot system    System used by the flora. See details.
#' @slot publisher Publisher of the flora
#' @slot date      Date flora published
#' @slot ISBN      ISBN of the flora
#'
#' @details
#'
#' `system` can be one of "Cronquist", "APG1", "APG3", or "Unknown"
#'
#' `key` must be a data.tree object returned by the define_key function.
#'
#' TODO: key should probably be its own class that includes the data.tree class
#'
#' `species` is a list of binomial names of format "Genus_species", where
#' each entry in the list is a taxon-class object.
#'
#' @author Brandon McNellis
#'
#' @name flora
#' @rdname flora
NULL
#' @rdname flora
#' @export
flora <- setClass(
  'flora',
  slots = list(

    name = 'character',
    key = 'data.tree',
    species = 'list',
    state = 'character',
    system = 'character',
    publisher = 'character',
    date = 'Date',
    ISBN = 'integer'

  )
)
#' @export
setValidity('flora', function(object) {
  errors <- character()

  if (FALSE) {
    msg <- paste0('error message')
    errors <- c(errors, msg)
  }

  if (!(object@system %in% .allowableSystems())) {
    msg <- paste0('Taxonomic system not recognized')
    errors <- c(errors, msg)
  }

  if (!(object@name %in% .supportedFloras())) {
    msg <- paste0('Flora name not available.\n
                   Use PrintSupportedFloras()')
    errors <- c(errors, msg)
  }

  # returns
  if (length(errors) == 0) {
    TRUE
  } else {
    errors
  }
})
#' @rdname flora
#' @export
setMethod('initialize',
          signature(.Object = 'flora'),
          function (.Object, ...) {
            params <- list(...)

            if ('system' %in% names(params)) {
              .Object@system <- params$system
            } else {
              # This is just here as an example check
              .Object@system <- 'Unknown'
            }

            .Object <- callNextMethod()
            mt <- validObject(.Object)
            if (isTRUE(mt)) {
              return(.Object)
            } else {
              return(mt)
            }
          }
)
#' @rdname flora
#' @export
.allowableSystems <- function() {
  allowable_systems <- c(
    'Cronquist',
    'APG1',
    'APG3',
    'Unknown'
  )

  return(allowable_systems)
}
#' @rdname flora
#' @export
.supportedFloras <- function() {
  supported_floras <- c(
    'test_Iris_key'
  )

  return(supported_floras)
}
#' @rdname flora
#' @export
PrintSupportedFloras <- function() {

  cat('\n')
  message("List of floras supported in the floRa package:")
  cat('\n\n')

  supported_floras <- .supportedFloras()
  print(supported_floras)

  invisible()

}
