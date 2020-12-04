#' @title S4 definition for taxon-class objects
#'
#' @description
#'
#' taxon-class objects store taxonomic ranks for use in the floRa package
#'
#' @slot binomial Binomial (Genus + Species) name
#'
#' @slot variety   Variety
#' @slot species   Species
#' @slot subgenus  Subgenus
#' @slot genus     Genus
#' @slot tribe     Tribe
#' @slot subfamily Subfamily
#' @slot family    Family
#' @slot order     Order
#' @slot division  Division
#'
#' @slot citation ICN Author citation
#' @slot taxon    Full taxonimic citation
#'
#' @details
#'
#' @author Brandon McNellis
#'
#' @name taxon
#' @rdname taxon
NULL
#' @rdname taxon
#' @export
taxon <- setClass(
  'taxon',
  slots = list(

    binomial = 'character',

    variety = 'character',
    subspecies = 'character',
    species = 'character',
    subgenus = 'character',
    genus = 'character',
    tribe = 'character',
    subfamily = 'character',
    family = 'character',
    order = 'character',
    division = 'character'

  )
)
#' @export
setValidity('taxon', function(object) {
  errors <- character()

  if (FALSE) {
    msg <- paste0('error message')
    errors <- c(errors, msg)
  }

  # returns
  if (length(errors) == 0) {
    TRUE
  } else {
    errors
  }
})
#' @rdname taxon
#' @export
setMethod('initialize',
          signature(.Object = 'taxon'),
          function (.Object, ...) {
            params <- list(...)

            if ('species' %in% names(params)) {
              .Object@species <- params$species
            } else {
              # This is just here as an example check
              .Object@species <- ''
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
