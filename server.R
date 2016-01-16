# PREDICTV3 This is the server.R file for the Shiny app for Next Word Prediction. 
# THe app can be seen at https://sanjeevv1.shinyapps.io/predictv3
# The language model used by the app (files r2 through r5 and m2 through m5) was created by a separate program also in this repo

library(dplyr)
library(ggplot2)
library(tm)
library(SnowballC)
library(caret)
library(wordcloud)
library(RWeka)
library(stringr)
library(tseries)
library(MASS)

r5 <- readLines("data/r5") 
r4 <- readLines("data/r4")
r3 <- readLines("data/r3")
r2 <- readLines("data/r2")

m5 <- as.matrix(read.table("data/m5"))
m4 <- as.matrix(read.table("data/m4"))
m3 <- as.matrix(read.table("data/m3"))
m2 <- as.matrix(read.table("data/m2"))

shinyServer(
  function(input, output) {
 # create help text window and text
	output$predict <- renderText({
output$helptext <- renderText({"Welcome to the next word prediction app. This app. will attempt to predict the next word in a sequence of words that
the user types in the input box. A couple of comments to help clarify its operation:

1. There may be an initial lag the very first time that you load the app as it loads the various 
data elements into memory that are needed to make the next word prediction. But that should only 
be noticeable the very first time. When you see the words 'Please enter input', in the 
'Our Prediction' tab that will be an indication that the app is ready to accept your input. 

2. The app generates one top word as its prediction, but additionally it gives a few more chocies. 
Typically, you will see between 1 to 5 choices for the next word. 

3. The app will only display its next word prediction in this version. The user will not be able 
to accept the word prediction as input to add to the text stream that the user is typing out. 

4. The app also attempts to identified typographical mistakes in the last word typed, and if that happens 
you will see a message to that effect. Some words that the user may think of as typos, may not, 
sometimes, be identified as such because they happen to be in the in-memory dictionary/tables.

5. Acceptable input is the english alphabet (case insensitive). Numbers etc are not acceptable input.

6. A profanity filter has been implemented, so profane words in user input will be flagged as typos.
Also, the next word prediction will not generate profane words. The list of profane words was taken from
the Shutterstock web site. "})

output$predict2 <- renderText({" "})  # some initializations
output$predict3 <- renderText({" "})
entries <- 0

input$goButton 			# wait for the "submit" button to be clicked
x <- isolate(tolower(input$text))  # convert text input to lower case
a <- str_split(x,boundary("word"))[[1]] # split input into separate words

if (length(a) == 0) {predict <- "Please Enter Input"} # if no input entered then ask for it
	else {predict <- "Looks like there may be a typo"} # if something is input, initialize prediction to "typo"

######################################################################################################
#                INPUT PHRASE >= 4 WORDS LONG FOR 5GRAM
#######################################################################################################

if (length(a) >=4) {       # if four or more words entered then execute this block for 5-gram processing

	# form search pharase with last two wrds and appended blank for 5-gram
	phrase <- paste0("^",a[length(a)-3]," ",a[length(a)-2]," ",a[length(a)-1]," ",a[length(a)]," ")  
	entries <- grep(phrase,r5)            # search for phrase in rownames of 5-gram matrix and save indices

	if (length(entries) >0){              # if there is a match in 5 gram then
	
	totals <- data.frame(entries,as.numeric(m5[entries]))  # save total counts for the matches as data frame
	names(totals) <- c("r5","m5")							# and name the data frame coumns for ordring

	totals <- totals[with(totals, order(-m5)),]  # sort on counts in decreasing order             

		if (length(entries) >= 1) { 	# save first choice for preidction
			aa <- str_split(r5[totals[1,1]],boundary("word"))
			choice1 <- sapply(aa,"[",5)
			predict <- paste0("1.",choice1)			
		}

		if (length(entries) >= 2) { 	# save second choice for prediction and create "other choices" display
			aa <- str_split(r5[totals[2,1]],boundary("word"))
			choice2 <- sapply(aa,"[",5)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2)})
		}
	
		if (length(entries) >= 3) { 	# save thrid choice for prediction
			aa <- str_split(r5[totals[3,1]],boundary("word"))
			choice3 <- sapply(aa,"[",5)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3)})
		}
		
		if (length(entries) >= 4) {    	# save fourth choice for presdiction
			aa <- str_split(r5[totals[4,1]],boundary("word"))
			choice4 <- sapply(aa,"[",5)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3,"  4.", choice4)})
		}
		
		if (length(entries) >= 5) { 	# save fifth choice for prediction
			aa <- str_split(r5[totals[5,1]],boundary("word"))
			choice5 <- sapply(aa,"[",5)
			predict <- paste0("1.",choice1)
			output$predict2 <- renderText("Other Choices Are:")
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3,"  4.",choice4,"  5.",choice5)})
		}
	}
}

###########################################################################################
##                              INPUT PHRASE = 3 WORDS LONG
###########################################################################################

if (length(a) ==3 | length(entries) == 0) {                   # if three words entered then execute this block for 4gram processing
	phrase <- paste0("^",a[length(a)-2]," ",a[length(a)-1]," ",a[length(a)]," ")  # form search pharase with last two wrds and appended blank for trigram
	#print(paste0(phrase, " in 4 gram"))
	entries <- grep(phrase,r4)            # search for phrase in rownames of trigram matrix
	#print(paste0("# matches ", length(entries)))
	if (length(entries) >0){              # if there is a match then
	
	totals <- data.frame(entries,as.numeric(m4[entries]))
	names(totals) <- c("r4","m4")

	totals <- totals[with(totals, order(-m4)),]  # sort on counts in decreasing order             
#print(paste0("class totals and dim totals",class(totals),dim(totals)))
		if (length(entries) >= 1) { #print("in first if")
			aa <- str_split(r4[totals[1,1]],boundary("word"))
			choice1 <- sapply(aa,"[",4)
			predict <- paste0("1.",choice1)
			#print(paste0("1. ",choice1," ",totals$m4[1]))
		}

		if (length(entries) >= 2) { 
			aa <- str_split(r4[totals[2,1]],boundary("word"))
			choice2 <- sapply(aa,"[",4)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2)})
			#print(paste0("2. ",choice2," ",totals$m4[2])) 
		}
	
		if (length(entries) >= 3) { 
			aa <- str_split(r4[totals[3,1]],boundary("word"))
			choice3 <- sapply(aa,"[",4)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3)})
			#print(paste0("3. ",choice3," ",totals$m4[3]))
		}
		
		if (length(entries) >= 4) { 
			aa <- str_split(r4[totals[4,1]],boundary("word"))
			choice4 <- sapply(aa,"[",4)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3,"  4.", choice4)})
			#print(paste0("4. ",choice4," ",totals$m4[4]))
		}
		
		if (length(entries) >= 5) { 
			aa <- str_split(r4[totals[5,1]],boundary("word"))
			choice5 <- sapply(aa,"[",4)
			predict <- paste0("1.",choice1)
			output$predict2 <- renderText("Other Choices Are:")
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3,"  4.",choice4,"  5.",choice5)})
			#print(paste0("5. ",choice5," ",totals$m4[5]))
		}
	}
}
#############################################################################################
##                            INPUT PHRASE = 2 WORDS LONG
################################################################################################
if (length(a) ==2 | length(entries) == 0) {                   # if two or more words entered then execute this block for trigram processing
	phrase <- paste0("^",a[length(a)-1]," ",a[length(a)]," ")  # form search pharase with last two wrds and appended blank for trigram
	#print(paste0(phrase, " in trigram"))
	entries <- grep(phrase,r3)            # search for phrase in rownames of trigram matrix
	#print(paste0("# matches ", length(entries)))
	if (length(entries) >0){              # if there is a match then
	#print(paste0("before totals assignemnt and (length(entries)) is ",length(entries)))
	totals <- data.frame(entries,as.numeric(m3[entries]))
	#print("after totals assignemtn")
	names(totals) <- c("r3","m3")
#print(paste0("after names assignemtn and class(totals$m3) is ",class(totals$m3)))
	totals <- totals[with(totals, order(-m3)),]  # sort on counts in decreasing order             
#print(paste0("class totals and dim totals",class(totals),dim(totals)))
#print("after class totals")
		if (length(entries) >= 1) { #print("in first if")
			aa <- str_split(r3[totals[1,1]],boundary("word"))
			choice1 <- sapply(aa,"[",3)
			predict <- paste0("1.",choice1)
			#print(paste0("1. ",choice1," ",totals$m3[1]))
		}

		if (length(entries) >= 2) { 
			aa <- str_split(r3[totals[2,1]],boundary("word"))
			choice2 <- sapply(aa,"[",3)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2)})
			#print(paste0("2. ",choice2," ",totals$m3[2])) 
		}
	
		if (length(entries) >= 3) { 
			aa <- str_split(r3[totals[3,1]],boundary("word"))
			choice3 <- sapply(aa,"[",3)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3)})
			#print(paste0("3. ",choice3," ",totals$m3[3]))
		}
		
		if (length(entries) >= 4) { 
			aa <- str_split(r3[totals[4,1]],boundary("word"))
			choice4 <- sapply(aa,"[",3)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3,"  4.", choice4)})
			#print(paste0("4. ",choice4," ",totals$m3[4]))
		}
		
		if (length(entries) >= 5) { 
			aa <- str_split(r3[totals[5,1]],boundary("word"))
			choice5 <- sapply(aa,"[",3)
			predict <- paste0("1.",choice1)
			output$predict2 <- renderText("Other Choices Are:")
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3,"  4.",choice4,"  5.",choice5)})
			#print(paste0("5. ",choice5," ",totals$m3[5]))
		}
	}
}
if ( length(a) == 1 | length(entries) ==0 ) {
#print("entered bigram")
if (length(a) == 0) {predict <- "Please Enter Input"}
#	else {predict <- "Looks like there may be a typo"}
	phrase <- paste0("^",a[length(a)]," ")
	entries <- grep(phrase,r2)
	if (length(entries) != 1053594) {
	#print(paste0("# of entries is ",length(entries)))
	if (length(entries) >0) {
		totals <- data.frame(entries,m2[entries])
		names(totals) <- c("r2","m2")
		totals <- totals[with(totals, order(-m2)),]
		             
		if (length(entries) >= 1) {
			aa <- str_split(r2[totals[1,1]],boundary("word"))
			choice1 <- sapply(aa,"[",2)
			predict <- paste0("1. ",choice1)
			#print(paste0("1. ",choice1," ",totals$m2[1]))
		}

		if (length(entries) >= 2) { 
			aa <- str_split(r2[totals[2,1]],boundary("word"))
			choice2 <- sapply(aa,"[",2)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2)})
			#print(paste0("2. ",choice2," ",totals$m2[2])) 
		}

		if (length(entries) >= 3) { 
			aa <- str_split(r2[totals[3,1]],boundary("word"))
			choice3 <- sapply(aa,"[",2)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3)})
			#print(paste0("3. ",choice3," ",totals$m2[3]))
		}
		
		if (length(entries) >= 4) { 
			aa <- str_split(r2[totals[4,1]],boundary("word"))
			choice4 <- sapply(aa,"[",2)
			predict <- paste("1.",choice1)
			output$predict2 <- renderText({"Other Choices Are:"})
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3,"  4.", choice4)})
			#print(paste0("4. ",choice4," ",totals$m2[4]))
		}
		
		if (length(entries) >= 5) { 
			aa <- str_split(r2[totals[5,1]],boundary("word"))
			choice5 <- sapply(aa,"[",2)
			predict <- paste0("1.",choice1)
			output$predict2 <- renderText("Other Choices Are:")
			output$predict3 <- renderText({paste0("2. ",choice2,"  3.",choice3,"  4.",choice4,"  5.",choice5)})
			#print(paste0("5. ",choice5," ",totals$m2[5]))
		}

	}

}}  
predict
}
)}
)

