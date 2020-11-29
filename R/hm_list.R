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
#' @export t_vari.hm.list
t_vari.hm.list <- setClass("t_vari.hm.list", contains = "hm.list")

#' @title time invariable s4 class
#' @importFrom methods new
#' @export t_invari.hm.list
t_invari.hm.list <- setClass("t_invari.hm.list", contains = "hm.list")

