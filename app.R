library(sbo)
library(tidytext)
library(ggplot2)
library(flexdashboard)
library(shiny)
library(tidyr)
library(dplyr)
library(sentimentr)
library(wordcloud)
library(RColorBrewer)

load("badwords.Rdata")
load("small_train_freq.Rdata")
load("sentim.Rdata")

ui <- fluidPage(

    titlePanel("Word Predictor"),
    fluidRow(
        column(width = 4, 
               p("Type a phrase and press Enter to see the top 3 suggested words,
                 the sentiment of the phrase, and a barplot and wordcloud
                 most likely next word"),
                textInput(inputId = "phrase", label = "Enter phrase", 
                      value = "i've had the best"),
                submitButton(text = "Enter"),
               br(),
               strong("Suggested Words:"),
               em(textOutput("word")),
               hr(),
               textOutput("sentiment"),
               gaugeOutput("sentimentPlot")),
        column(width = 4, plotOutput("barPlot")),
        column(width = 4, plotOutput("wordCloud"))
        )
    )

server <- function(input, output) {
    
    p <- reactive({
        s <- sentiment(input$phrase)
        pt <- predict(small_train_freq, input$phrase)
        colnames(pt) <- c("word", "probability")
        pt <- anti_join(pt, badwords, by = "word") %>%
            left_join(sentim, by = "word") %>%
            filter(word != "<EOS>")
        if(s$sentiment > -0.25 & s$sentiment < 0.25) { 
            pt[is.na(pt)] <- 1
            pt <- arrange(pt, desc(probability), desc(value)) 
        } else { pt[is.na(p)] <- 0
        if(s$sentiment > 0.25){ pt <- arrange(pt, desc(probability), desc(value))
        } else { pt <- arrange(pt, desc(probability), value)} }  
        pt
    })
    
    s <- reactive({
        st <- sentiment(input$phrase) %>%
            mutate(nnp = if(sentiment > -0.22 & sentiment < 0.22) { "Neutral" }
                   else { ifelse(sentiment > 0.22, "Positive", "Negative") })
        st
    })
    
    output$word <- renderText({
        p <- p()
        paste(p$word[1], " , ", p$word[2], " , ", p$word[3])})
    
    output$sentiment <- renderText({
        s <- s()
        paste("The phrase has a mostly", tolower(s$nnp), "sentiment") })
    
    output$sentimentPlot <- renderGauge({
        s <- s()
        s$sentiment <- round(s$sentiment, 3)
        gauge(s$sentiment, min = -1, max = 1, label = s$nnp, 
              gaugeSectors(success = c(0.22, 1), 
                           warning = c(-0.22, 0.22), 
                           danger = c(-1, -0.22)))
    })
    
    output$barPlot <- renderPlot({
        p <- p()
        id <- seq(1:20)
        pg <- cbind(id, p[c(1:20), c(1:2)])
        pg$probability <- pg$probability * 300
        
        label_data <- pg
        angle <- 90 - 360 * (label_data$id-0.5) / 20
        label_data$hjust <- ifelse(angle < -90, 1, 0)
        label_data$angle <- ifelse(angle < -90, angle + 180, angle)
        
        ggplot(pg, aes(x=as.factor(id), y = probability, fill = probability)) +
            geom_bar(stat = "identity", alpha = 0.5) +
            ylim(-50, 120) + labs(title="Top 20 Words by Proability") +
            theme_minimal() +
            theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  panel.grid = element_blank(),
                  legend.position = "none", 
                  plot.margin = unit(rep(-4,4), "cm")) +
            coord_polar(start = 0) + 
            geom_text(data = label_data, aes(x = id, y = probability + 10,
                                             label = word, hjust = hjust), 
                      color = "black",
                      fontface = "bold", alpha = 0.6, size = 4, 
                      angle = label_data$angle, inherit.aes = FALSE)
    })
    
    output$wordCloud <- renderPlot({
        p <- p()
        set.seed(555)
        wordcloud(words = p$word, freq = p$probability*1000, 
                  min.freq = 1, max.words=100, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
    })
}

shinyApp(ui = ui, server = server)
