#' @title t_vari.array s4 class
#' @importFrom methods new
#' @export t_vari.array
t_vari.array <- setClass("t_vari.array", contains = "array")

#' @title hm.list s4 class
#' @importFrom methods new
#' @export hm.list
hm.list <- setClass("hm.list", contains = "list")

#' @title time variable s4 class
#' @importFrom methods new
#' @export t_vari.list
t_vari.list <- setClass("t_vari.list", contains = "hm.list")

#' @title time invariable s4 class
#' @importFrom methods new
#' @export t_invari.list
t_invari.list <- setClass("t_invari.list", contains = "t_vari.list")

