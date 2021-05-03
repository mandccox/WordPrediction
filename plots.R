
### Circlular Bar Plot
library(ggplot2)
id <- seq(1:20)
pg <- cbind(id, p[c(1:20), c(1:2)])
pg$probability <- pg$probability * 1000

label_data <- pg
angle <- 90 - 360 * (label_data$id-0.5) / 20
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)
        
barplot <- ggplot(pg, aes(x=as.factor(id), y = probability, fill = probability)) +
              geom_bar(stat = "identity", alpha = 0.5) +
              ylim(-50, 120) +
              theme_minimal() +
              theme(axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank(),
                    plot.margin = unit(rep(-2,4), "cm")) +
              coord_polar(start = 0) + 
              geom_text(data = label_data, aes(x = id, y = probability + 10,
                            label = word, hjust = hjust), color = "black",
                            fontface = "bold", alpha = 0.6, size = 2.5, 
                            angle = label_data$angle, inherit.aes = FALSE)

### Word Cloud
library(wordcloud)
library(RColorBrewer)
set.seed(5555)
wordcloud(words = p$word, freq = p$probability*1000, 
          min.freq = 1, max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

### Gauge
library(flexdashboard)
s <-sentiment(phrase) %>%
      mutate(nnp = if(sentiment > -0.25 & sentiment < 0.25) { "Neutral" }
        else { ifelse(sentiment > 0.25, "Positive", "Negative") })
gauge(s$sentiment, min = -1, max = 1, label = s$nnp, gaugeSectors(success = c(0.25, 1), 
                warning = c(-0.25, 0.25), danger = c(-1, -0.25)))
  