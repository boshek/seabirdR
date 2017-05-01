#' @title Read in Seabird .cnv files that have common parameters
#' @description This function is meant to import raw cnv files into an R data frame.
#' @export
#'
#' @param autoname Should the function attempt to extract the names of the variables from raw data or should they be left blank and specified by the user using colnames()? Defaults to TRUE
#' @param recursive Should the function look into sub-folders of the current folder?
#'
#' @return A single dataframe containing all the cast dale and a label column indicating the filename of the .cnv file


read_seabird = function(path = ".", autoname = TRUE, recursive = FALSE) {
  #if(length(list.files(path = ".", pattern="*.cnv", recursive=recursive)) == 0L) {
  #  message("No .cnv files in search path. Use setwd() to navigate to data directory") } else{

  ## Create ctddf to input into
  ctddf <- c()
  #x <- list.files(pattern="*.cnv", recursive=FALSE) ##DEBUG
  ## Loop through and find all the files
  for (x in list.files(path = path, pattern="*.cnv", recursive=recursive)) {
    ##Read in entire .cnv file
    all_content = readLines(paste0(path, x), warn=FALSE)

    if(autoname == TRUE) {
      ## Automatically find files name TODO: Could be much more efficient
      tmpdf = gsub("\t","",all_content[grep("# name",all_content)])
      ## Trimming possible white space
      fnames = trimws(sub('\\:.*','', sub('.*\\=', '', tmpdf)))
      ## Read all lines before "*END*"
      skip = all_content[-c(1:max(grep("*END*",all_content)))]
      #input <- tryCatch(expr = read.table(textConnection(skip), col.names = fnames),
      #                  message = "Files in the search patterns have different parameters")
      input <- read.table(textConnection(skip), col.names = fnames)

      ##Test against autoread being problematic
      stopifnot(length(input)==length(fnames))
    } else {
      #print("Use colnames to manually assign column names")
      input <- read.table(textConnection(skip))
    }

    input$Label = factor(gsub(".*/", "", x)) ##Regexes the filename specifically
    #tryCatch(rbind(ctddf, input), "Incorrect name specification; try autoname=FALSE")
    ctddf <- rbind(ctddf, input)
  }
    message(paste0(length(unique(ctddf$Label))," of ",
         paste0(length(list.files(path = path, pattern="*.cnv", recursive=recursive))), " cnv files in the search path successfully imported"))
    return(ctddf)
#}
}
