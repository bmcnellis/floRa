### run it!
library(floRa)
data(iris)

Iris_setosa <- new('taxon')
Iris_setosa@binomial <- 'Iris setosa'
Iris_setosa@genus <- 'Iris'
Iris_setosa@species <- 'setosa'

Iris_versicolor <- new('taxon')
Iris_versicolor@binomial <- 'Iris versicolor'
Iris_versicolor@genus <- 'Iris'
Iris_versicolor@species <- 'versicolor'

Iris_virginica <- new('taxon')
Iris_virginica@binomial <- 'Iris virginica'
Iris_virginica@genus <- 'Iris'
Iris_virginica@species <- 'virginica'

# BEM note: needs a trait-class object that can be included for each taxon

iris$treeclassifier <-
  floRa::evaluate_tree(tree = tr.iris, indata = iris)


iris[c(48:54,130:135), ### row numbers picked to show what happens with multiple species
     c("Petal.Width",
       "Species",
       "treeclassifier")]
