#' zero_var
#'
#' A small internal function which determines for each column in
#' a dataframe or matrixf the variance is 0 or not
#'
#' @param m dataframe or matrix
#' @return a boolean vector
#' @keywords internal

zero_var <- function(m){
  apply(m, 2, function(v) {
    var(v) != 0
  })
}

#' decbin
#'
#' converts a deciman number into a bit array (represented by integers)
#'
#' @param decnum the deciman number
#' @return an integer vector to represent a bitarray
#' @keywords internal

decbin <- function(decnum) {
  if (decnum < 2) {
    return(decnum)
  }
  c(decbin((decnum %/% 2)),decnum %% 2)
}


#' decpad
#'
#' pads an array with zeros left until the lengths of the array is equal to len
#'
#' @param decnum the deciman number
#' @param len length of the output array
#' @return a padded array of length len
#' @keywords internal

decpad <- function(decnum, len){
  binrep <- decbin(decnum)
  c(rep(0, len - length(binrep)), binrep)
}


#' get_start_node
#'
#' gets the start node for CIRM. This will be the leaf with maximum
#' success-rate.
#'
#' @param frm Output of rp$frame
#' @return The start node index
#' @keywords internal

get_start_node <- function(frm){
  nfrm <- frm[frm$yval == 2,]
  nfrm <- nfrm[nfrm[,"var"] == "<leaf>",]
  pot_start <- as.numeric(row.names(nfrm))[which.max(nfrm$n)]
  pot_start
}


#' get_parent_node
#'
#' gets the start node for CIRM. This will be the leaf with maximum
#' success-rate.
#'
#' @param node_id The id of the node we want to know the parent
#' @return parent id
#' @keywords internal

get_parent_node <- function(node_id){
  # Each level the nodes has index 2*(previous_level) + i
  # So the parent node will be the floored integer after dividing the
  # index by two
  as.integer(node_id/2)
}


#' parse_rule_row
#'
#' This simple function parses a rule at a node represented by a string like
#' wt>=2.393 Rule is a list named after the variable (e.g wt) used in the
#' spliting. The list consists a numeric vector what contains information about
#' the spliting: the splitting point and the direction. The direction is the
#' first value: 1 if >= 2 else 2. The second part is the splitting point
#'
#' @param string
#' @return rule
#' @keywords internal

parse_rule_row <- function(string){
  rule_row  <- regmatches(string,regexec("([a-zA-Z_0-9]+)([>=< ]+)(.*)",string,
                                         perl=TRUE))[[1]][-1]
    if(rule_row[2] == ">="){
      rule_num <- 1
    } else {
      rule_num <- 2
    }
    rule <- list(c(rule_num,as.numeric(rule_row[3])))
    names(rule) <- rule_row[1]
    return(rule)
}


#' change_parameters_on_node
#'
#' Changing the parameters on a node based on the splitting rule on it's label
#'
#' @param nodes All nodes specified by labels
#' @param node Current node
#' @param parameters2 Parameter dataframe
#' @return parameter dataframe
#' @keywords internal

change_parameters_on_node <- function(nodes,node,parameters2){
  crule <- parse_rule_row(nodes[as.character(node)])
  minmax <- unlist(parameters2[parameters2[,1] == names(crule),c(3,4)])
  if(crule[[1]][1] == 1){
    if(minmax[1]<=crule[[1]][2]){
      minmax[1] <- crule[[1]][2]
      if(minmax[1] <= minmax[2]){
        parameters2[parameters2[,1] == names(crule),c(3,4)] <- minmax
      } else {
        write(sprintf("WARNING: %s's  minimum(%s) > maximum(%s)",
                      parameters2[,1], minmax[1], minmax[2]),  "errorlog.txt",
              append=TRUE)
      }
    }
  } else {
    if(minmax[2]>=crule[[1]][2]){
      minmax[2] <- crule[[1]][2]
      if(minmax[1] <= minmax[2]){
        parameters2[parameters2[,1] == names(crule),c(3,4)] <- minmax
      } else {
        write(sprintf("WARNING: %s's  minimum(%s) > maximum(%s)",
                      parameters2[,1], minmax[1], minmax[2]),
                      "errorlog.txt", append=TRUE)
      }
   }
  }
  parameters2
}


#' update_parameters_based_on_tree
#'
#' Update parameter intervals based on the decesion tree.
#'
#' @param rp rpart made decesion tree
#' @param parameters parameter dataframe
#' @return parameter dataframe
#' @export

update_parameters_based_on_tree <- function(rp, parameters){
  frm <- rp$frame
  nodes <- labels(rp)
  names(nodes) <- row.names(frm)
  node <- get_start_node(frm)
  parameters <- change_parameters_on_node(nodes,node,parameters)
  while(node !=1){
    node <- get_parent_node(node)
     if(node == 1){
       break()
     }
    parameters <- change_parameters_on_node(nodes,node,parameters)
  }
  parameters
}

#' svg_html
#'
#' This function plots the ˙code˙ in an svg graphic environment, reads the
#' temparary file created, and after that it returns the svg string element.
#'
#' @param code The code used for plot making
#' @export

svg_html <- function(code){
  tmp <- tempfile()
  svg(tmp)
    code
  dev.off()
  paste(readLines(tmp)[-1],collapse='')
}


#' glue
#'
#' This function is the general GLUE method
#'
#' @param results The result.csv file after one montecarlo iteration
#' @param res_r comparison results
#' @param output the output file name
#' @param epcname The name of the epc file
#' @export


glue <- function(results="result.csv",res_r="results.RDS",
               output ="gplot.pdf",epcname="maize_glue.epc"){
  res <- read.csv(results)[-1]
  res <- res[-1,]
  colnames(res)
  non_zero <- res[,1:(ncol(res)-4)]
  colnames(non_zero) <- colnames(res)[1:(ncol(res)-4)]
  impvars <- zero_var(non_zero)
  nonzero <- non_zero[,impvars]
  likelihoods <- res[,(ncol(res)-3)]
  rmse <- res[,(ncol(res)-2)]
  const <- res$Const
  namess <- gsub("__.*","",colnames(nonzero))
  likelihoods <- likelihoods[res$Const==1]
  goods <- res[res$Const==1,]
  medlik <- median(likelihoods[likelihoods >= quantile(likelihoods,0.95)])
  medlik_place <- which.min(abs(likelihoods - medlik))
  parameters <- readRDS(res_r)
  glue_opt <- goods[medlik_place, 1:(ncol(res)-4)][impvars]
  nonka <- goods[likelihoods >= quantile(likelihoods,0.95),1:(ncol(res)-4)]
  med_opt <- apply(nonka,2,median)[impvars]
  # med_opt <- apply(nonka,2,mean)[impvars]
  ml_opt <- goods[which.max(likelihoods),1:(ncol(res)-4)][impvars]
  calibrationPar <- parameters$calibrationPar[impvars]
  changemulline(src="maize.epc", calibrationPar = calibrationPar,
                contents=glue_opt, outFiles = epcname)
  changemulline(src="maize.epc", calibrationPar = calibrationPar,
                contents=med_opt, outFiles = "maize_median.epc")
  changemulline(src="maize.epc", calibrationPar = calibrationPar,
                contents=ml_opt, outFiles = "maize_ml.epc")
  print(output)
  pdf(output)
  for(i in 1:ncol(nonzero)){
    plot(nonzero[,i],res[,(ncol(res)-3)],main="",col="lightgray",
         pch=20, cex=0.4, xlab=namess[i],ylab="logLikelihood")
    points(nonzero[const==1,i],res[const==1,(ncol(res)-3)],
           pch=20, cex=0.6, col="red",type="p",
           xlab=namess[i],ylab="logLikelihood")
    abline(v=glue_opt[i],col="green")
    abline(v=med_opt[i],col="blue")
    abline(v=ml_opt[i],col="black")
  }
  dev.off()
}

#' cirmi_glue_agromo
#'
#' This is the AgroMo implementation of CIRMI_GLUE
#'
#' @param parameters parameter intervallum dataframe
#' @param measurements measurements table
#' @param mc_iterations measurements table
#' @param cirm_iterations measurements table
#' @param likelihood measurements table
#' @param execPath measurements table
#' @param constraints measurements table
#' @param th measurements table
#' @param max_succ_rate measurements table
#' @param pb The progressbar object we update, use NULL with shiny
#' @param pbUpdate The progressbar update function
#' @param copyThread should the function delete the temporal folder
#' and copy the necessary files over?
#' @param dataVar The variable we want to calibrate
#' @return parameter dataframe
#' @export

cirmi_glue_agromo <- function(parameters, measurements, mc_iterations,
                            cirm_iterations, likelihood, execPath,
                            constraints, th, max_succ_rate,
                            pb, pbUpdate, copyThread, dataVar) {
  to_out <- list(treesvgs = vector(mode = "character", cirm_iterations),
                 parameters = vector(mode = "list", cirm_iterations))
  for (cirm_i in 1:cirm_iterations) {
    results <- RBBGCMuso::multiSiteCalib(measurements = measurements,
                            parameters = parameters,
                            calTable = calTable,
                            dataVar = dataVar,
                            iterations = as.numeric(isolate(input$paranait)),
                            pb = NULL,
                            pbUpdate = function(x) {
                                           setProgress(value = x, detail = x)
                                                   },
                            likelihood = likelihood,
                            execPath = execPath,
                            copyThread = input$copyThread,
                            constraints = constraints, th = th)
    len <- round(log(max(results$failType), 2))
    failTypes <- do.call(rbind, lapply(results$failType, function(x) {
        decpad(x, len)
    }))
    nonzero <- results[, 1:(ncol(results) - 4)]
    nonzero <- nonzero[, -1]
    nonzero <- nonzero[, zero_var(nonzero)]
    colnames(nonzero) <- gsub("__.*", "", colnames(nonzero))
    images <- vector(mode = "character", len)
    for (const in 1:len) {
      constraint <- failTypes[, const]
      baseTable <- cbind.data.frame(nonzero,
                                    constraint = as.factor(constraint))
      try({
          rp = rpart(constraint ~ ., data = baseTable)
      })
      try({
          parameters <- update_parameters_based_on_tree(rp, parameters)
      })
      try({
          images[const] <- svg_html(rpart.plot(rp))
      })
    }
    to_out$treesvgs[cirm_i] <- paste(images, collapse = "")
    to_out$parameters[[cirm_i]] <- parameters
  }
  return(to_out)
}
