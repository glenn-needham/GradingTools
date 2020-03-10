suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stringr))

# This simply opens the csv and directs to the interface function
loadCSV <- function(filenam){
  datu <<-read.csv(filenam)
  datu$name <<- as.character(datu$name)
  datu$Nweek <<- as.factor(datu$Nweek)
  datu$Class <<- as.factor(datu$Class)
  datu$semester <<- as.factor(datu$semester)
  interface()
}

# This function just takes the data frame and generates graphs
interface <- function(){
  spectra <- readline(prompt = "Are you interested a student (s), or overall (o) grades: ")
  if(spectra == 'o'){
    draft <- ggplot(data = datu, aes(x = Class, 
                                     y = grade, 
                                   color = Nweek))+
      geom_boxplot(outlier.shape = NA, aes(color = Nweek))+
      #geom_jitter(width=0.2, position_dodge())+
      theme_classic()
    print(draft)
  } else if (spectra == 's'){
    namen <- readline(prompt = "What is the student's name: ")
    #print(namen)
    shortList <- subset(datu, grepl(namen, datu$name))
    #print(shortList)
    if (length(unique(shortList$name))<1){
      cat(paste0("No on by the name ", namen, " found. Try again?"))
      cat("\n")
    } else if (length(unique(shortList$name))>1){
      cat("Multiple hits found in the dataset.")
      cat("\n")
      print(unique(shortList$name))
      namen <- readline(prompt = "Which one did you want data for: ")
      shortList <- subset(datu, grepl(namen, datu$name))
    }
    if (length(unique(shortList$name)) == 1){
      cat(paste0("Generating data for ", unique(shortList$name), "."))
      cat("\n")
      typo <- readline(prompt = "Would you like absolute grades (a) or over time (t): ")
      if (typo == "a"){
        draft <- ggplot(data = datu, aes(x=assignType, y=grade))+
          geom_boxplot(outlier.shape = NA, aes(color = assignType))+
          geom_jitter(width=0.2)+
          theme_classic()
        print(draft)
        meanGrade <- mean(shortList$grade)
        cat(paste0(unique(shortList$name), "'s grade average is ", round(meanGrade, digits = 2), "%"))
      } else if (typo == "t"){
        draft <- ggplot(data = datu, aes(x=date, y=grade))+
          geom_bar(stat = "identity")+
          theme_classic()
        print(draft)
        meanGrade <- mean(shortList$grade)
        cat(paste0(unique(shortList$name), "'s grade average is ", round(meanGrade, digits = 2), "%"))
      } else {
        cat("Sorry, unable to generate those records. Try again?")
        cat("\n")
      }
    } else {
      cat("Sorry, unable to locate records with that name.")
      cat("\n")
    }
  }
}