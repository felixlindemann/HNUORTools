# environment in package namespace used to save package
# settings
.HNUORToolsEnv <- new.env()
assign("settings",  list(), envir = .HNUORToolsEnv)
assign("nodes",  list(), envir = .HNUORToolsEnv)
assign("customers",  list(), envir = .HNUORToolsEnv)
assign("warehouses",  list(), envir = .HNUORToolsEnv)


.onAttach <- function(lib, pkg){
  packageStartupMessage(
          "------------------------------------------------",
        "\n HNUORTools Version ",  utils::packageDescription("HNUORTools", field="Version"), 
        "\n Tools for Operationsresearch Tutorium",
        "\n For an introduction visit: http://felixlindemann.github.io/HNUORTools/",
        "\n CAUTION: The package is in alpha phase.",
        "\n          Design changes may still occur.", 
        "\n------------------------------------------------", 
        appendLF = TRUE)
  
  # invisible object saved in environment in namespace 
}
