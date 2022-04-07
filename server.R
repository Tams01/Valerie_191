#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

make_url_html <- function(url) {
    if(length(url) < 2) {
        if(!is.na(url)) {
            as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
        } else {
            ""
        }
    } else {
        paste0(map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
    }
}

# Define server logic required to draw a histogram
library(twitteR)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #Save app credentials for access to tweets
    consumerKey <- "f22IKpVCBM53CgsIsKAS1K6XD"
    consumerSecret <- "HfVQDoPfACj1ztxQ5p2VhSrpdyjVRvOvMWSANmGwGkLWqvtDfr"
    accessToken <- "1388381999715475465-oqaQt6ajWzxMHZQRcZFjgGVoW41OUB"
    accessTokenSecret <-  "dfinr1AxaGppt5P54DIut3z6Cbkml1QCjQKjgsZ1lsJgC"
    #set up 
    setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
    
    #initial file information
    #a <- file.info(list.files(path="C://Users//SIMIYOUNG//Desktop//R folder//Shiny App//TFW",pattern=".csv", full.names=TRUE))$atime
    
    #schedule this collector to run after 10000 milliseconds    
    
    
    observe({
        invalidateLater(10000, session = NULL)
        #data collector
        data <- searchTwitter("9mobile", n = 30, 
                              resultType = "recent", lang = "en")
        
        ?searchTwitter
        #data %>% twListToDF() %>% 
        write.table(twListToDF(data), "myDF.csv", sep = ",", col.names = !file.exists("myDF.csv"), append = T, row.names = F)  
        
        # if (a != file.info(list.files(path="C://Users//SIMIYOUNG//Desktop//R folder//Shiny App//TFW",
        #                                   pattern=".csv", full.names=TRUE))$atime){
        # df_csv <- read.csv("myDF.csv")
        # }
        #file.remove("myDF.csv")
        
        
    })
    
    # read.csv("https://raw.githubusercontent.com/simmieyungie/Corona-Virus/master/App/nrc.csv") %>% 
    #   write.csv("nrc.csv")
    # #   
    # observe({})
    # get_sentiments("nrc")
    
    data <- reactivePoll(60000 * 2, session = NULL,
                         # This function returns the time that log_file was last modified
                         checkFunc = function() {
                             if (file.exists("myDF.csv"))
                                 file.info("myDF.csv")$mtime[1]
                             else
                                 ""
                         },
                         # This function returns the content of log_file
                         valueFunc = function() {
                             
                             # 
                             # tokeep <- 10
                             # 
                             # df_size <- R.utils::countLines("TFW//myDF.csv")
                             read.csv("myDF.csv") %>% 
                                 as.data.frame() %>% 
                                 filter(!is.na(id)) %>% 
                                 filter(is.na(replyToSID))
                             # %>%
                             #   filter(!is.na(lubridate::ymd_lubridate::hms(created)))
                             #filter(!is.na(strptime(date,format="%Y-%m-%d 00:00:00"))) %>% 
                             # mutate(len = nchar(created)) %>% 
                             # filter(len == 19) %>% 
                             # drop_na(created) %>% 
                             # select(-len)
                             # 
                             
                             
                         }
    )
    
    
    #render value box outputs
    output$`Number of Tweets` <- renderValueBox({
        #Get the death rate
        
        valueBox(data() %>% 
                     nrow(), subtitle = "Number of Tweets",
                 icon = icon("twitter-square", lib ="font-awesome" ), color = "red")
        
    })
    
    
    output$`Number of Retweets` <- renderValueBox({
        
        #get the number of retweets
        retweets <-  data() %>% 
            mutate(retweet = str_detect(text, "RT")) %>%
            filter(retweet == T) %>%
            #separate(date, into = c("date", "time"), sep = " ") %>% 
            count(retweet)
        valueBox(retweets[,2], subtitle = "Number of retweets", 
                 icon = icon("retweet", lib = "font-awesome"), color = "blue")
    })
    
    #Number of mentions
    output$`Number of Handles` <- renderValueBox({
        
        handles <-  data() %>% 
            summarise(n = n_distinct(screenName))
        
        #render box
        valueBox(handles, subtitle = "Number of Handles", 
                 icon = icon("at", lib = "font-awesome"), color = "blue")
        
    })
    
    
    # render value box for number of handles
    output$`Number of Mentions` <- renderValueBox({
        #Get number of tweets
        Mentions <- users(data()$text) %>% 
            unlist() %>%
            tolower() %>% 
            as_tibble() %>% 
            count()
        #render box
        valueBox(Mentions, subtitle = "Number of Mentions", 
                 icon = icon("globe", lib = "font-awesome"), color = "blue")
        
    })
    
    #render word cloud
    output$wordcloud <- renderHwordcloud({
        cloud <- data() %>% 
            mutate(rowmumber = row_number()) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>% 
            mutate(text = gsub("https","", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
            mutate(text = removeNumbers(text)) %>% 
            mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            count(word, sort = T) %>% 
            top_n(40) %>% 
            drop_na()
        
        hwordcloud::hwordcloud(text = cloud$word, size = cloud$n,
                               theme = "gridlight", )
        
    })
    
    #top 10 words
    output$top10 <- renderHighchart({
        cloud <- data() %>% 
            mutate(rowmumber = row_number()) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>% 
            mutate(text = gsub("https","", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
            mutate(text = removeNumbers(text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            count(word, sort = T) %>% 
            top_n(10) %>% 
            drop_na()
        cloud %>% 
            hchart("bar", hcaes(x = word, y = n))
    })
    
    output$Polarity <- renderPlotly({
        data_format <- data() %>% 
            mutate(text = iconv(text)) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(row_id = row_number()) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("brt", "", text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            count(sentiment, row_id, screenName) %>% 
            spread(sentiment, n, fill = 0) %>%  # made data wide rather than narrow
            mutate(sentiment = positive - negative)
        
        All_banks <- data() %>%
            mutate(row_id = row_number())
        
        data_format <- data_format %>% 
            left_join(All_banks, by = "row_id")
        
        label_wrap <- label_wrap_gen(width = 60)
        
        data_format2 <- data_format %>% 
            rename(screenName = screenName.x) %>% 
            select(-screenName.y) %>% 
            mutate(text_formatted = str_glue("Row ID: {row_id}
                                   Screen Name: {screenName}
                                   Text: {label_wrap(text)} "))
        
        data_format3<- data_format2 %>% 
            select(1:5, "text_formatted")
        
        
        ggplotly(data_format3 %>% 
                     ggplot(aes(row_id, sentiment)) +
                     geom_line(color= "black", alpha = 0.5) +
                     geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
                     geom_hline(aes(yintercept = mean(sentiment), color = "blue")) +
                     geom_point(aes(text = text_formatted), color = "orange", shape = 5) +
                     geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "orange") +
                     geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "orange") +
                     theme_bw() +
                     labs(y = "sentiment score", x = "Twitter User"),
                 tooltip = "text") %>% 
            layout(
                xaxis = list(
                    rangeslider = list(type = "date")
                )
            )
    })
    
    tweet_table_data <- reactive({
        req(data())
        data_format <- data() %>% 
            mutate(text = iconv(text)) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(row_id = row_number()) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("brt", "", text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            count(sentiment, row_id, screenName) %>% 
            spread(sentiment, n, fill = 0) %>%  # made data wide rather than narrow
            mutate(sentiment = positive - negative)
        All_banks <- data() %>%
            mutate(row_id = row_number())
        
        data_format <- data_format %>% 
            left_join(All_banks, by = "row_id") %>% 
            rename(user_id = id,
                   created_at = created,
                   screen_name= screenName.y,
                   favorite_count = favoriteCount,
                   retweet_count = retweetCount,
                   urls_expanded_url = statusSource) %>% 
            mutate(status_id = user_id) %>% 
            select(user_id, created_at, status_id, screen_name, text, favorite_count, retweet_count, urls_expanded_url, sentiment) %>%
            mutate(
                Tweet = glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
                URLs = map_chr(urls_expanded_url, make_url_html)
            )%>%
            select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs, sentiment)
    })
    
    output$tweet_table <- renderReactable({
        reactable(tweet_table_data() %>% mutate(urgency = ifelse(sentiment < 0, "Negative", 
                                                                 ifelse(sentiment == 0, "Neutral", "Positive"))) %>% 
                      filter(urgency == "Negative"), 
                  filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                  showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 3, showPageSizeOptions = TRUE, pageSizeOptions = c(10, 50, 75, 100, 200), 
                  columns = list(
                      DateTime = colDef(defaultSortOrder = "desc"),
                      User = colDef(defaultSortOrder = "asc"),
                      Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                      Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                      RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                      URLs = colDef(html = TRUE),
                      sentiment = colDef(defaultSortOrder = "desc"),
                      urgency = colDef(defaultSortOrder = "desc")
                  )
        )
    })
})

library(glue)
library(purrr)
