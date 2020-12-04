#' @title Defines taxonomic keys
#'
#' @description NA
#'
#' @author Brandon McNellis
#' @export
define_key <- function(flora_name) {

  # Iris test key

  if (flora_name == 'test_Iris_key') {

    ### root ####
    tr.iris <- Node$new(
      name = "Root",
      nextvar = "Petal.Width",
      nextlogical = "Petal.Width < 1;
                 Petal.Width >= 1 & Petal.Width < 1.4;
                 Petal.Width >= 1.4 & Petal.Width < 1.9;
                 Petal.Width >= 1.9"
    )

    ###
    tr.iris$AddChild(
      name = "Narrow petals",
      result = "setosa"
    )

    tr.iris$AddChild(
      name = "Medium petals",
      result = "versicolor"
    )

    tr.iris$AddChild(
      name = "Medium-wide petals",
      result = "cannot classify by petal width alone"
    )

    tr.iris$AddChild(
      name = "Wide petals",
      result = "virginica"
    )

    return(tr.iris)

  } else {
    stop("key definition function failed")
  }

  invisible()

}
