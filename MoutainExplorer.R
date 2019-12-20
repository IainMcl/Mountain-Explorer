  # This is a command line based mountain exploration game.
  # I think this will help my R programming ...
  library(rapportools)
  library(stringr)
  library(stringi)
  
  ################### GLOBAL DISPLAY OPTIONS ########################
  
  displayText <- function(text, options=FALSE, clear=TRUE){
    # Add functionalit to display text in the command line in a fun way.
    # text - a string to be displayed
    # options - future optionsm, either FALSE or a list.
    
    # default while building.
    # Multi line functionality still not working with list of lines not updating properly
    
    # clear the console.
    if(text == ""){
      return()
    }
    if(clear){
      cat("\014")
    }
    n_new_lines <- 0
    if(str_detect(text, "\n")){
      # string contains new line which will mess things up.
      n_new_lines <- str_count(text, pattern = "\n")
    }
    
    max_line_length <- 80
    page_head <- rep("-", max_line_length + 11)
    cat(page_head, sep="")
    
    txt_len <- nchar(text)
    lines <- vector(mode="list", length=txt_len%/%max_line_length + 1 + n_new_lines)
    if(txt_len > max_line_length){
      line_counter <- 1
      cur_text_pos <- 1
      split_string <- strsplit(text, "")[[1]]
      for(i in 1:length(split_string)){
        if(i %% max_line_length == 0){
          lines[line_counter] <- substr(text, cur_text_pos, i-1)
          line_counter <- line_counter + 1
          cur_text_pos <- i
        }
        else if(split_string[i] == "\n"){
          # Add condition here for if char is "\n" 
          lines[line_counter] <- substr(text, cur_text_pos, i-1)
          line_counter <- line_counter + 1
          cur_text_pos <- i+1
        }
      }
      # Add remaining text to final line
      lines[line_counter+1] <- substr(text, cur_text_pos, length(strsplit(text, "")[[1]]))
    } else{
      lines[1] <- text
    }
    for(line in lines){
      if(is.empty(line)){
        next
      }
      line_len = nchar(line)
      padding <- (max_line_length - line_len + 10) %/% 2
      if(identical(padding, numeric(0))){
        padding <- 1
      }
      cat("\n")
      cat("|")
      cat(rep(" ", padding), sep="")
      cat(line)
      cat(rep(" ", padding), sep="")
      cat("|", sep="\n")
    }
    cat(page_head,sep="")
    cat("\n")
  }
  
  ################### GENERIC FUNCTIONS ########################
  
  #' Does this add documentation
  info <- function(){
    text <- ""
    displayText(text, clear=TRUE)
  }
  
  options <- function(opts){
    text <- str_join_list(opts, sep="\t")
    displayText(text, clear=TRUE)
  }
  
  setup <- function(){
    # cat("\014")  
    text <- "Welcome to Mountain Explorer."
    displayText(text, clear=TRUE)

    confirm <- FALSE
    while(!confirm){
      name <<- readline("What is your name? ")
      cat("Is that right? Your name is ")
      cat(name)
      cat("?", sep="\n")
      y_n <- tolower(readline())
      if(y_n == "yes" | y_n == "y"){
        confirm <- TRUE
      }
    }
    displayText("Lets begin our adventure.", clear=TRUE)
    cat("\n\n")
  }
  
  displayInventory <- function(){
    displayText("Inventory")
    if(!inventory){
      displayText("No items.")
    }
    inventory_string <- ""
    for(item in inventory){
      paste(inventory_string, item, sep=" - ")
    }
    displayText(inventory_string)
  }
  
  endGame <- function(){
    # output endgame text
  }
  
  exitGame <- function(){
    displayText("You have quit the game.")
    exit()
  }
  
  getInput <- function(options=FASLE){
    input <- tolower(readline(prompt="Choose what to do: "))
    if(input == "?"){
      info()
    }
    else if(input == "options"){
      options()
    }
    else if(input == "inventory"){
      displayInventory()
    }
  }
  
  recieveInput <- function(options){
    input <- trimws(tolower(readline("What do you do next? ")))
    if(input == "quit" | input == "exit"){
      exitGame()
    }
    else if(str_detect(input, "info") | str_detect(input, "?")){
      info()
    }
    else if(str_detect(input, "options") | str_detect(input, "opt")){
      i <- 1
      for(opt in options){
        paste0(i, ". ", opt)
        i <- i + 1
      }        
    }
    else if(input %in% options){
      # complete the option
      return(input)
    }
    else{
      return(NULL)
    }
    return(FALSE)
  }
  
  ####################### ROOMS #############################
  
  home <- function(){
    displayText("Home", clear=FALSE)
    displayText("Stepping out of the door you\n\n look around getting dazzled by the light. As your eyes slowly adjust several paths come into focus. To the North there is a Dark cave. To the east there is a long path entering the forrest. To the south is your home and to the East is a sharp Cliff edge.", clear=FALSE)
    input <- recieveInput(list("north", "east", "south", "west"))
    if(is.null(input)){
      cat("Invalid input. Try: options, info ...")
    }
  }
  
  darkCave <- function(){
    displayText("Dark Cave", clear=TRUE)
    displayText("You brush the cobwebs asside and enter the Dark Cave. Looking around you see only darkness.", clear=FALSE)
  }
  
  
  ##################### PLAYER #############################
  name <- ""
  standing = TRUE # FALSE = crouching
  current_room <- home
  inventory <- FALSE
  complete <- FALSE
  
  
  ##################### RUNNING #############################
  
  
#' Mountain Explorer
#'
#' @examples
  main <- function(){
    setup()
    counter <- 0
    while(!complete){
      if(counter >= 5){ # remove when working
        break
      }
      options <- current_room()
      recieveInput(options)
      counter <- counter + 1 # Remove when working
    }
    endGame()
    return(0)
  }
  
  main()


