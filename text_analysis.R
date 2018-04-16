# Author: Michael Rojas
# MA415/615
# 4/12/18

# President speech Analysis between Presient Trump and President Clinton

library(tidytext)
library(tidyverse)
library(dplyr)
library(scales)
library(stringr)
library(ggplot2)
library(wordcloud2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {  
  library(grid)  
  
  # Make a list from the ... arguments and plotlist  
  plots <- c(list(...), plotlist)  
  
  numPlots = length(plots)  
  
  # If layout is NULL, then use 'cols' to determine layout  
  if (is.null(layout)) {  
    # Make the panel  
    # ncol: Number of columns of plots  
    # nrow: Number of rows needed, calculated from # of cols  
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),  
                     ncol = cols, nrow = ceiling(numPlots/cols))  
  }  
  
  if (numPlots==1) {  
    print(plots[[1]])  
    
  } else {  
    # Set up the page  
    grid.newpage()  
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))  
    
    # Make each plot, in the correct location  
    for (i in 1:numPlots) {  
      # Get the i,j matrix positions of the regions that contain this subplot  
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))  
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,  
                                      layout.pos.col = matchidx$col))  
    }  
  }  
} 

##################################################################

# Trumps Speach Analysis
trump <- scan("trumptalk.txt", "")
trump
trump_df <- data_frame(line = 1:5190, text = trump)
trump_df

trump_text <- trump_df %>%
  unnest_tokens(word, text)

data(stop_words)

trump_text1 <- trump_text %>%
  anti_join(stop_words)

trump_text2 <- trump_text1 %>%
  count(word, sort = TRUE) 

trump_text2

#View(trump_text2)

trump_text3 <- trump_text2[c(-1,-7,-1294:-1301),]

# The following will display a cloud of words used throughout the speach with the most 
# frequently used words popping out as biggest (Notice: America, People, Tonight, Country..)

wordcloud2(trump_text3)

#########################################################

# Clintons Speach Analysis
clinton<-scan("clintontalk.txt", "")
clinton
clinton_df <- data_frame(line = 1:6959, text = clinton)
clinton_df

clinton_text <- clinton_df %>%
  unnest_tokens(word, text)

data(stop_words)

clinton_text1 <- clinton_text %>%
  anti_join(stop_words)

clinton_text2 <- clinton_text1 %>%
  count(word, sort = TRUE) 

#View(clinton_text2)

clinton_text3 <- clinton_text2[-1,]

# The following will display a cloud of words used throughout the speach with the most 
# frequently used words popping out as biggest (Notice: Plan, Country, Health, Jobs..)

wordcloud2(clinton_text3)

##################################################################
# Word frequency

# Word frequency Analysis for President Trump
pres1<-trump_text3 %>%
  filter(n > 7) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  print(ggtitle("President Trumps Speach Analysis")) +
  geom_col() +
  xlab("President Trump") +
  ylab("Word Frequency") +
  coord_flip() + 

  # Pretty Printing (Title,Axis Modifications)
theme(
  plot.title = element_text(family = "Helvetica", colour = "steelblue4", face = "bold", size = (20), hjust = 0.5),
  legend.title = element_text(family = "Helvetica", colour = "steelblue4", face = "bold"),
  axis.title = element_text(family = "Helvetica", face = "bold", size = (14), colour = "steelblue4")
) 

pres1

# Word frequency Analysis for President Clinton 
pres2<-clinton_text3 %>%
  filter(n > 7) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  print(ggtitle("President Clintons Speach Analysis")) +
  geom_col() +
  xlab("President Clinton") +
  ylab("Word Frequency") +
  coord_flip() +

  # Pretty Printing (Title,Axis Modifications)
theme(
  plot.title = element_text(family = "Helvetica", colour = "steelblue4", face = "bold", size = (20), hjust = 0.5),
  legend.title = element_text(family = "Helvetica", colour = "steelblue4", face = "bold"),
  axis.title = element_text(family = "Helvetica", face = "bold", size = (14), colour = "steelblue4")
) 
  

pres2

# The following compares both plots one on top of the other.
multiplot(pres1,pres2)

###################################################################

# Short Analysis Depictions

#   The graph shows that both presidents use words like America, People, Country and Tonight. 
#   However, President Clinton used more specific words like health, eduaction, jobs, energy..  
#          
#   It is easy to point out the simple and less diverse words used through out President Trumps speach
#   whereas we see more diversity in President Clinton's with a higher density in word
#   frequency.
#
#   This analysis shows the change in importance to what matters the most to the American people
#   in different eras from education, health and crisis in Clintons time as president to 
#   immigration and workers in Trumps presidency. 

###################################################################

# Repetitive rate analysis between trump and clinton
frequency1 <- bind_rows(mutate(trump_text3, president = "trump"),
                        mutate(clinton_text3, president = "clinton")) %>% 
  count(word , president) %>%
  group_by(president) %>%
  mutate(proportion = nn / sum(nn)) %>% 
  select(-nn) %>% 
  spread(key = president, value = proportion)

frequency1

frequency1[is.na(frequency1)] <- 0

# Comparison between first state of the union of clinton and trump 
ggplot(frequency1, aes(x = clinton, y = trump)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position = "none")

 # we dont get much useful information here.

#################################################################
## Sentiment analysis

# President Analysis Tables
president <- rep('clinton',2445)
clinton_text_s1 <- cbind(president,clinton_text1)
clinton_text_s1

president <- rep('trump',2182)
trump_text_s1 <- cbind(president,trump_text1)
trump_text_s1

total_text <- rbind(clinton_text_s1,trump_text_s1) %>% rename(index=line) %>% as.tibble()
total_text

#################################################################
# Sentiment table for both presidents

# Positive & negative analysis
president_sentiment <- total_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(president, index = index %/% 40, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(president_sentiment, aes(index, sentiment, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ president, ncol = 2, scales = "free_x")

##################################################################
# Short observation

# From the above graph, we can see that Trump actually used more postive words than clinton, 
# especially in the end of his speech, with its value raised to almost 7.5 on the sentiment meter,
# a number that clinton didnt reach.

##################################################################

# Nrc analysis
president_sentiment_nrc <- total_text %>%
  inner_join(get_sentiments("nrc")) %>%
  count(president, index = index %/% 40, sentiment) %>%
  spread(sentiment, n, fill = 0) 

##################################################################

# Joy analysis
ggplot(president_sentiment_nrc, aes(index, joy, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ president, ncol = 2, scales = "free_x")


##################################################################
# Short observation

# From the Joy graph above, we can see that Trump actually shows much more joy than 
# clinton in the speech at the end of his speach almost reaching up to values of 6 on the 
# sentiment meter but also throughout his whole speach as well. 


##################################################################
# Anger analysis

ggplot(president_sentiment_nrc, aes(index, anger, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ president, ncol = 2, scales = "free_x")

##################################################################
# Short observation

# From the Anger graph above, we can see that Trump shows more anger than clinton in his speech, 
# reaching up to levels of 5 around the middle of his speech according to the sentiment meter.
# While both of them calm down in the end, it is easy to depict that Clinton was happier that day
# than Trump.

##################################################################
# Anticipation analysis

ggplot(president_sentiment_nrc, aes(index, anticipation, fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ president, ncol = 2, scales = "free_x")

##################################################################

# From the anticipation graph above, we can see that Trump shows less anticipation than clinton 
# in his speech, reaching only up to levels of 4 max throuhg out his speech according to the 
# sentiment meter. It can be seen that President Clintons speech reached levels of anticipation 
# of up to a level of 5 more than once according to the sentiment meter.While both of them 
# up to 2 on the anticpation meter President clintons speeach wins this round.