library(shiny)
library(tm)
suppressWarnings(library(stringr))

# Load RDS files which cleaning and ngram was done on the data for predictive analysis
#bigramfile <- file("C:/Users/yulun/Desktop/coursera R programming/Capstone/capstone_project_final/bigram.RData")
bigram_rds <- readRDS("bigram.RData")
#close(bigramfile)
#trigramfile <- file("C:/Users/yulun/Desktop/coursera R programming/Capstone/capstone_project_final/trigram.RData")
trigram_rds <- readRDS("trigram.RData")
#close(trigramfile)
#quadgramfile <- file("C:/Users/yulun/Desktop/coursera R programming/Capstone/capstone_project_final/quadgram.RData")
quadgram_rds <- readRDS("quadgram.RData")
#close(quadgramfile)

testing <<- ""
# Function for obtaining predicted word from rds
# Back off algorithm is used
## First, quadgram will be used. The last three words from a sentence is used to predict the next word
## If no match is found, then trigram will be used. The last two words from a sentence is used to predict the next word.
## Finally, if no match is found aain, a bigram will be used. The last word from the sentence is used to predict the next word.
get_word <- function(sentence) {
    # Just in case 
    sentence <- tolower(sentence)
    sentence <- removePunctuation(sentence)
    sentence <- removeNumbers(sentence)
    # split
    splitted <- strsplit(sentence, " ")[[1]]
    # Quadgram
    if (length(splitted) >= 3) {
        # Quadgram: Get last 3 words from the sentence
        words_tbu <- tail(splitted,3)
        # Finding exact match from data
        matched <- head(quadgram_rds[quadgram_rds$unigram == words_tbu[1] & quadgram_rds$bigram == words_tbu[2] & quadgram_rds$trigram == words_tbu[3], 4],1)
        # If no match, get last two words from the sentence and repeat
        if (identical(character(0), matched)){
            ##???get_word(paste(words_tbu[2],words_tbu[3],sep=" "))
            trigram_try <- paste(words_tbu[2],words_tbu[3],sep=" ")
            splitted <- strsplit(trigram_try, " ")[[1]]
            words_tbu <- tail(splitted,2)
            matched <- head(trigram_rds[trigram_rds$unigram == words_tbu[1] & trigram_rds$bigram == words_tbu[2], 3],1)
            if (identical(character(0), matched)){
                bigram_try <- paste(words_tbu[2])
                splitted <- strsplit(bigram_try, " ")[[1]]
                words_tbu <- tail(splitted, 1)
                matched <- head(bigram_rds[bigram_rds$unigram == words_tbu[1], 2],1)
                if (identical(character(0), matched)){
                    testing <<- {"No match found, the most common word 'the' is returned"; head("the", 1)}
                }
                else {
                    testing <<- {head(bigram_rds[bigram_rds$unigram == words_tbu[1], 2])}
                }
            }
            else {
                testing <<- {head(trigram_rds[trigram_rds$unigram == words_tbu[1] & trigram_rds$bigram == words_tbu[2], 3])}
            }
        }
        else {
            testing <<- {head(quadgram_rds[quadgram_rds$unigram == words_tbu[1] & quadgram_rds$bigram == words_tbu[2] & quadgram_rds$trigram == words_tbu[3], 4])}
        }
    }

    else if (length(splitted) == 2) {
        words_tbu <- tail(splitted,2)
        matched <- head(trigram_rds[trigram_rds$unigram == words_tbu[1] & trigram_rds$bigram == words_tbu[2], 3],1)
        if (identical(character(0), matched)){
            bigram_try <- paste(words_tbu[2])
            splitted <- strsplit(bigram_try, " ")[[1]]
            words_tbu <- tail(splitted, 1)
            matched <- head(bigram_rds[bigram_rds$unigram == words_tbu[1], 2],1)
            if (identical(character(0), matched)){
                testing <<- {"No match found, the most common word 'the' is returned"; head("the", 1)}
            }
            else {
                testing <<- {head(bigram_rds[bigram_rds$unigram == words_tbu[1], 2])}
            }
        }
        else {
            testing <<- {head(trigram_rds[trigram_rds$unigram == words_tbu[1] & trigram_rds$bigram == words_tbu[2], 3])}
        }
        
    }
    
    else if (length(splitted) == 1) {
        words_tbu <- tail(splitted, 1)
        matched <- head(bigram_rds[bigram_rds$unigram == words_tbu[1], 2],1)
        if (identical(character(0), matched)){
            testing <<- {"No match found, the most common word 'the' is returned"; head("the", 1)}
        }
        else {
            testing <<- {head(bigram_rds[bigram_rds$unigram == words_tbu[1], 2])}
        }
        
    }
}
new_sentence <- "prince"
get_word(new_sentence)

# Define server logic required to predict next word
shinyServer(function(input, output) {

    output$pred_word <- renderPrint({
        predicted_words <- get_word(input$Input_for_prediction)
        #output$pred_word <- renderText({testing})
        #predicted_words
        testing
    })

})

