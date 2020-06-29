#' @title viewFuPa
#' @description view the funtions oder parameters
#' @import purrr
#' @param Name the Name of funtions oder parameters
#' @param clsNa the class name of S3 function
#' @return for function return the Input data and Out data structure
#' for paramter return the wert and description
#' @export
viewFuPa <- function(Name, clsNa){
  if(is.function(Name)) {
    Ret <- Name(runMode = "VIEW")
  } else {
    PaD <- paste0("ParamDescript$", Name)
    PaW <- paste0("ParamAll$", Name)
    Des <- eval(parse(text = PaD))
    Ret <- eval(parse(text = PaW))
    cat(paste0(Des, "\n"))
  }
  return(Ret)
}

#' @title viewData
#' @description view the structure and names of the elements
#' @import purrr
#' @param Model a variable in list
#' @param name the name of the variable
#' @return NULL, the message will on Console print
#' @export
viewData <- function(Model, name){
  cat(paste0("The |", name, "|\t(at least) include:\t", paste(names(Model), collapse = ", "), ".\n"))
  judgeDL <- as.logical(map(Model, function(A) is.data.frame(A) | is.list(A)))
  if(any(judgeDL)){
    Model <- Model[which(judgeDL)]
    cat(paste0("In the |", name, "| ", paste(names(Model), collapse = ", ")," is(are) lists or data.frames.\n"))
    map2(Model, names(Model), viewData)
  }
  return()
}

#' @title checkData
#' @description check the structure and names of the elements
#' @import purrr
#' @param Aim a variable in list
#' @param Check the will be checkd variable
#' @param name the name of the variable
#' @return NULL, the message will on Console print
#' @export
checkData <- function(Aim, Check, name){
  cat(paste0("Checking ", name, " now:\n"))
  # browser()
  AimNa <- names(Aim)
  CheNa <- names(Check)
  judgeCT <- AimNa %in% CheNa
  if(all(judgeCT)) {
    judgeDL <- as.logical(map(Aim, function(A) is.data.frame(A) | is.list(A)))
    Aim0 <- Aim[which(!judgeDL)]
    Aim <- Aim[which(judgeDL)]
    Mess0 <- paste(names(Aim0), collapse = ", ")
    cat(paste0(Mess0,"\n", CheckOkMess, BoundryString))
    if(length(Aim) != 0) {
      cat(paste0("Checking lists or data.frames in ", name, ":\n"))
      AimNa <- names(Aim)
      Check <- Check[which(CheNa %in% AimNa)]
      pmap(list(Aim, Check, AimNa), checkData)
    }
  } else {
    message("ERROR: ", name, " missing: ", paste(AimNa[which(!judgeCT)], collapse = ", "), ".")
  }
  message(checkNote)
  return()
}

# eval(parse(text = paste0(fcName, "(", paste("Arguments$", names(Arguments), sep = "", collapse = ", "), ")")))
# viewArgum <- function(fcName, Arguments) {
#   cat(paste0(BoundryString, viewNote, BoundryString, "Function ", fcName, " has the following requirements:\n"))
#   vw <- viewData(Arguments, "Arguments")
#   cat(paste0(BoundryString, "Function ", fcName, " will return:\n"))
#   Out <- eval(parse(text = paste0(fcName, "(", paste("Arguments$", names(Arguments), sep = "", collapse = ", "), ")")))
#   vw <- viewData(Out, "Out")
#   cat(BoundryString)
#   return(Out)
# }

##*## second methond for viewArgum
#' @title viewArgum
#' @description view the Arguments of a function
#' @importFrom utils str
#' @param fcName function name
#' @param Arguments Arguments
#' @return return a list, includ Arguments and outdata,
#' they have the same structure whith Arguments and outdata, but not the real werts
#' @export
viewArgum <- function(fcName, Arguments) {
  cat(paste0(BoundryString, viewNote, BoundryString, "Function ", fcName, " has the following requirements:\n"))
  vw <- str(Arguments, "Arguments")
  cat(paste0(BoundryString, "Function ", fcName, " will return:\n"))
  Out <- eval(parse(text = paste0(fcName, "(", paste("Arguments$", names(Arguments), sep = "", collapse = ", "), ")")))
  vw <- str(Out, "Out")
  cat(BoundryString)
  return(Out)
}










