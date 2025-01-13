#' @export
TestClass <- R6::R6Class(
  "TestClass",
  public = list(
    initialize = function(secrets_dir = ".secrets") {
      message("Initializing with secrets_dir: ", secrets_dir)
      invisible(self)
    }
  )
) 