install.packages("https://cran.r-project.org/src/contrib/Archive/MuMIn/MuMIn_1.46.0.tar.gz", repos = NULL, type="source")
library(MuMIn)


# import data-loader file
source("Code/data-loader.R")


#Temp code - Cleaning quarter column
df <- subset(load_datasets(), select = -c(time_frame))
df$quarter <- as.numeric(substr(df$quarter, 2, nchar(df$quarter)))
str(df)


#Printing correlation plot 
library(GGally)
ggpairs(df)



#Edit here 
model_data <- df                        #Specify cleaned dataset 
targetcolumn <- "fatality_number"       #Spicify target column
ignorecolumns<- c("1","a")              #Specify columns to ignore if any. Otherwise all other columns are used for input


# **Code to automatically read column names and input to model, no need to change anything here
allcols=colnames(model_data)
removecoles=append(ignorecolumns,targetcolumn)
inputcols <- allcols[! allcols %in% removecoles]

colstring <- targetcolumn
colstring <- paste(colstring," ~ ",sep="")
for(colname in inputcols ){
  colstring <- paste(colstring,colname,sep="")
  colstring <- paste(colstring," + ",sep="")
}
colstring = substring(colstring,1, nchar(colstring)-3)
print(colstring)



# Change to desired model here
globalmodel <- glm.nb(eval(parse(text = colstring)),
                   data = model_data,
                   na.action = "na.fail")


#Dredging
combinations <- dredge(globalmodel,rank="AIC")

#Printing model for all combinations of input columns
print(combinations)

#Summary of top performing model
top_model <- get.models(combinations, subset = 1)[[1]]
top_model
summary(top_model)

#summary of average of top performing models with delta less then 2 of top model
summary(model.avg(combinations, subset = delta <= 2))

#Stepwise 

library(MASS)

#Forward
step.model.forward <- stepAIC(globalmodel, direction = "forward", 
                      trace = FALSE)
summary(step.model.forward)

#Backward
step.model.backward <- stepAIC(globalmodel, direction = "backward", 
                      trace = FALSE)
summary(step.model.backward)

#Both
step.model.both <- stepAIC(globalmodel, direction = "both", 
                      trace = FALSE)
summary(step.model.both)
