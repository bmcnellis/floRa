### run it!
library(floRa)
data(iris)

tr.iris <- floRa::define_key_Iris()

iris$treeclassifier <-
  floRa::evaluate_tree(tree = tr.iris, indata = iris)


iris[c(48:54,130:135), ### row numbers picked to show what happens with multiple species
     c("Petal.Width",
       "Species",
       "treeclassifier")]
