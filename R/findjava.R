#' find java path , solve the rjava problem
#
#'@param
#'@return all of your java paths
#'@examples
#' findjava
#' Sys.setenv(JAVA_HOME='C:\\Your\\Java\\Directory')
#' library(rJava)
#'@export
#'@name findjava
#'
#'
#'
#'

findjava <- function() {
  for (root in c("HLM", "HCU")) for (key in c("Software\\JavaSoft\\Java Runtime Environment",
                                              "Software\\JavaSoft\\Java Development Kit")) {
    hive <- try(utils::readRegistry(key, root, 2),
                silent = TRUE)
    if (!inherits(hive, "try-error"))
      return(hive)
  }
  hive
}
