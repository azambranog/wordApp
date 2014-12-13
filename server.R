library(tm)
library(data.table)
library(wordcloud)
i=1
f4.final <-  readRDS(paste("f4final2_", i, '.Rds', sep = ''))

clean1 <- function(a){
    a <- gsub("\\.\\.\\.", "", a)
    a <- strsplit(a, "(\\.|\\?|!|\\(|\\)|\\[|\\]|\\{|\\})")
    a <- unlist(a)
    a <- gsub("(-|@|/|#|~|$|%|&|+|=|:|;|,\\|<|>)", " ", a)
    a <- gsub("( arse | ass )", "butt", a)
    a <- gsub("ballsack", "scrotum", a)
    a <- gsub("(bastard|cunt)", "stupid", a)
    a <- gsub("(bitch|biatch|slut|whore)", "bad", a)
    a <- gsub("(blowjob| blow job )", "oral", a)
    a <- gsub(" boner ", "erection", a)
    a <- gsub("(boob| tit )", "breast", a)
    a <- gsub("( cock |dick)", "penis", a)
    a <- gsub("shit", "crap", a)
    a <- gsub(" fag |faggot","homosexual", a)
    a <- gsub("fucking", "kidding", a)
    a <- gsub("fuck", "hell", a)
    a <- gsub("(nigger|nigga)", "black", a)
    a <- gsub("pussy", "vagina", a)
    a <- gsub(" wank ", "masturbate", a) 
    return(a)
}

clean2 <- function(a) {
    a <- gsub("[^[:alnum:]&^']", " ", a)
    return(a)
}

clean3 <- function(input){
    sapply(strsplit(input, ' '), function(x) paste0(rle(x)$values, collapse=' '))
}

predict.word <- function(input, stop.word=F, 
                         BCD = 150, CD = 90, BC= 35, 
                         ABC=20, D=20){
    x<-Corpus(VectorSource(input))
    x <- tm_map(x, clean1)
    x <- tm_map(x, clean2)
    x <- tm_map(x, tolower)
    x <- tm_map(x, removeNumbers)
    x <- tm_map(x, removePunctuation)
    x <- tm_map(x, stripWhitespace)
    x <- tm_map(x, clean3)
    x <- tm_map(x, PlainTextDocument)
    
    q <- strsplit(paste(x$content[[1]][1]$content, collapse = ""), " ")[[1]]
    q <- q[q!=""]
    l<- length(q)
    
    
    if(l==0) {
        message("you did not input anything valid")
    }
    if(l>=1){#D
        solution <-f4.final[third == paste(q[l], collapse = " "), ]    
        solution <- solution[ ,sum(p), by = ending][order(V1, decreasing=T), ]
        solution<-solution[, V1:=(D/3277)*V1]
        
    }
    if(l>=2){#CD
        solution2 <- f4.final[second.third == paste(q[(l-1):l], collapse = " "), ]   
        solution2 <- solution2[ ,sum(p), by = ending][order(V1, decreasing=T), ]
        solution2<-solution2[, V1:=V1*(CD/98.5)]
        solution<-rbindlist(list(solution,solution2))
    }
    if(l>=3){
        #BCD
        solution2 <-f4.final[root == paste(q[(l-2):l], collapse = " "), ]
        solution2 <- solution2[ ,sum(p), by = ending][order(V1, decreasing=T), ]
        solution2<-solution2[, V1:=V1*(BCD/.36)]
        solution<-rbindlist(list(solution,solution2))
        
        #BC
        solution2 <- f4.final[first.second == paste(q[(l-2):(l-1)], collapse = " "), ]   
        solution2 <- solution2[ ,sum(p), by = ending][order(V1, decreasing=T), ]
        solution2<-solution2[, V1:=V1*(BC/71.27)]
        solution<-rbindlist(list(solution,solution2))
    }
    if(l>3){
        #ABC
        solution2 <- f4.final[root == paste(q[(l-3):(l-1)], collapse = " "), ]   
        solution2 <- solution2[ ,sum(p), by = ending][order(V1, decreasing=T), ]
        solution2<-solution2[, V1:=V1*(ABC/.36)]
        solution<-rbindlist(list(solution,solution2))
        
    }
    
    if(stop.word==T){
        solution<-solution[ ,sum(V1), by = ending][order(V1, decreasing=T), ]    
    }
    else{
        solution<-solution[ ,sum(V1), by = ending][order(V1, decreasing=T), ]    
        solution<-solution[!(ending%in%stopwords("en")),]    
    }
    
    return(solution)
}

    
shinyServer(function(input, output) {
    a<-reactive ({ 
        input$predict
        isolate({
            sol <<- predict.word(input)
        })
        })
    
    output$cloud <- renderPlot({
        a()
        wordcloud(sol[,ending], sol[,(V1/min(V1))], max.words=25 , 
                  scale = c(4, 1))
        
    })
    output$solution <- renderPrint({
        a()
        print(data.table(sol[1:5,ending]))
    })
    
})