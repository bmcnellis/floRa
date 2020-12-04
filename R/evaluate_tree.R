#' @title Tree evaluate function
#'
#' @description NA
#'
#' @param tree   Object of class `data.tree`. See details.
#' @param indata `data.frame` containing columns with names matching all the
#'               values of "nextvar" used in the `tree` object.
#'
#' @details
#'
#' Tree nodes must contain these attributes:
#'
#' "nextvar", specifying the variable name to evaluate when picking a path down. Not needed in end nodes.
#' "nextlogical", specifying the logical evaluations to look at when picking the next path down. Not needed in end nodes.
#' "result", specifying the value returned. END NODES ONLY!
#'
#' @author Joe Brehm
#'
#' @export
evaluate_tree <- function(tree, indata) {

  outlist <-
    sapply(1:nrow(indata),
           function(row){

             # start at the root node
             node = tree$root

             # descend through the tree until arriving at an end node
             while(!isLeaf(node))  {

               # extract the name of the variable to evaluate
               varname = node$nextvar

               # check to see if its in the input data
               if(!(varname %in% colnames(indata))) {
                 stop(paste(varname, "must be a column name in input data"))
               }

               # extract the value
               value = indata[row,varname]

               # if the value is na, this point can't be evaluated due to missing data unless there is a check for NA's in the logic set
               if(is.na(value) & !grepl("is.na", node$nextlogical)) return(NA)

               # change the logical string to use the generic 'value' instead of the specified variable name
               # specific variable names are used in the tree objects for readability
               lstr = gsub(varname, "value", node$nextlogical)

               # go from the semicolon delimited list of logicals to a vector
               v.lstr <- unlist(strsplit(lstr, split = ";"))

               # evaluate them all
               v.bool <- sapply(v.lstr, function(l){eval(parse(text = l))})

               # which is true?
               pathno <- which(v.bool)

               # if 0 or >1 paths are true, there is an error in the tree
               if(length(pathno) != 1){
                 stop(paste0(
                   "Node '", node$pathString, "' has ", sum(v.bool), " solutions, should be 1"
                 ))
               }

               # if there are more logicals to evaluate than there are children, there is an error in the tree
               if(length(v.bool) > length(node$children)) {
                 stop(paste0(
                   "Node '", node$pathString, "' has ", length(v.bool), " logical statements but ", length(node$children), " children"
                 ))
               }

               # otherwise, take that path
               node = node$children[[pathno]]
             } # loop back to evaluate the new node now -- or exit, if its a leaf

             return(node$result) # send back to the row-level eval
           }
    ) # end of the sapply, returns a list of end classes

  out <- unlist(outlist)
  return(out)
}
