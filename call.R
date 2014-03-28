install_load <- function (package1, ...) 
{
  # convert arguments to vector
  packages <- c(package1, ...)
  
  for(package in packages){
    
    # if packages exists, load into environment
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package)) 
    
    # if package does not exist, download, and then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
    
  }
  
}

install_load("devtools")
install_load("shiny")

runGitHub("hra", "heinizon")

#runApp("D:/users/jturner1/Desktop/RShinyDemo/")
# R -e "shiny::runApp('D:/users/jturner1/Desktop/RShinyDemo')"
