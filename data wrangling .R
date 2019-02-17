library(tidyverse)
library(dslabs)

#getting the data from extdata directory
path <-  system.file("extdata",package="dslabs")
#looking at the files included in the directory
list.files(path)
fullpath<- file.path(path,"murders.csv")
#moving the file to the working directory
file.copy(fullpath ,getwd())
# checking if the file exists in the working directory
file.exists(path)
#reading the first 3 line of a data file
read_lines("murders.csv",n_max=3)
#tidyverse function
dat <- read_csv("murders.csv")
dat
class(dat)
#R based function
dat2 <- read.csv( "murders.csv")
class(dat2)
#R convers charecter to factor to avoid the we type
dat3<- read.csv("murders.csv",stringsAsFactors = F)
class(dat3$abb)
# converting gapminder(non tidy data) to tidy data
filename <- file.path(path,"fertility-two-countries-example.csv" )
wild_data <- read_csv(filename)
wild_data
#using gather function to reshape the data from wild to tidy data
new_tidy_data <- wild_data %>% gather(year,fertility,`1960`:`2015`)
new_tidy_data
#other way to write the code is wild_dat %>% gather (year, fertility ,-country )
# noticing that the in new data that year has been converted to charecter to avoid this we type
class(new_tidy_data$year)
new_tidy_data <- wild_data %>% gather(year,fertility , - country,convert = TRUE)
class(new_tidy_data$year)
# using spread function to convert from tidy to wild data
new_wild_data <- new_tidy_data %>% spread(year,fertility)
#convert a wild to tidy data
fullpath2 <- file.path(path,"life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(fullpath2)
raw_dat
# noticeed that that there is two underscore which cause NA value so we used the arhument extra to merge the extra underscores
dat4 <- raw_dat %>% gather( key , value , -country , convert = TRUE) 
dat4 <- dat4 %>% separate( key ,c("year","variable_name") , sep = "_", extra = "merge")
dat4 <- dat4 %>% spread(variable_name , value)
dat4

#loading a file from html 
url <- "https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state"
h <- read_html(url)
class(h)
tab <- h %>% html_nodes("table")
tab <- tab[[2]]
head(tab)
tab <- tab %>% setNames(c("state","population","total","murders","gun_murders"
,"gun_ownership","total_rate","murder_rate","gun_murder_rate"))
head(tab)
head(murders_raw)
murders_raw <- murders_raw[[2]] #I have no idea what this line does

#\ is for escaping 
s <- "5'10\""
cat(s)

#converting character from html to numeric 
#using as.numeric() will not work
as.numeric(murders_raw$population[1:3])
#using as.numeric() will not work because of the commas
as.numeric(murders_raw$population[1:3])
#using as.numeric() will not work because of the commas
commas <- function(x)any(str_detect(x,","))
murders_raw %>% summarise_all(funs(commas))
identical(test_1,test_2)
test_1 <- str_replace_all(murders_raw$population,",","")
test_1 <- as.numeric(test_1)
#than we We can then use the mutate_all to apply this operation to each column,
#since it won't affect the columns without commas.
#using parse_number function does all that in one line of code
test_2 <- parse_number(murders_raw$population)
identical(test_1,test_2)
head(murders_raw)
#1 detecting commas
data(reported_heights)
data(reported_heights)
class(reported_heights$height)
as.numeric(reported_heights$height)
x <- as.numeric(reported_heights$height) # NA value was reported so it can't be solved by this function
sum(is.na(x))
# keep only NA entry
reported_heights %>% mutate(new_height =as.numeric(height))%>%
+ filter(is.na(new_height)) %>% head()
not_inches <- function(x, smallest = 50, tallest = 84) {
inches <- suppressWarnings(as.numeric(x))
ind <- is.na(inches) | inches < smallest | inches > tallest
ind
}
x <- as.numeric(reported_heights$height) # NA value was reported so it can't be solved by this function
sum(is.na(x))
not_inches(x)

animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "moo*"
str_detect(animals, pattern)

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")


converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

#detect the psttern and replace it 
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]

#case study: exrtacting a table from a pdf 
#downloading a pdf file 
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)
raw_data_research_funding_rates <- txt[2]
#or we can load it from dslabs
data("raw_data_research_funding_rates")

raw_data_research_funding_rates %>% head
#ach line on the page, including the table rows, is separated by the symbol for newline: \n

tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
tab %>% head
#we see that the information for the column names is the third and forth entires:
the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1
#We want to remove the leading space and everything following the comma. We can use regex for the latter. 
#Then we can obtain the elements by splitting using the space. We want to split only when there are 2 or more spaces to avoid splitting success rate.
#So we use the regex \\s{2,} as follows:
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2
#Now we can join these to generate one name for each column
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

#Now we are ready to get the actual data. By examining the tab object, 
#we notice that the information is in lines 6 through 14. 
#We can use str_split again to achieve our goal:
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

#We can see that the objects are identical:
identical(research_funding_rates, new_research_funding_rates)

#Dates and times 
library(dslabs)
data("polls_us_election_2016")
class(polls_us_election_2016$startdate)
head(as.numeric(polls_us_election_2016$startdate))

#visualizition data 
polls_us_election_2016 %>% filter(pollster =="Ipsos" & state =="U.S." ) %>% 
  ggplot( aes(startdate , rawpoll_trump)) +geom_line()

library(lubridate) 
month(polls_us_election_2016$startdate , label=TRUE )  
#Another useful set of functions are the parsers that converts strings into dates.
#the preferred format is iso 8601 (yyyy-mm-dd)
x <- c(20090101 , "2009-01-02","2009 01 03", "2009-1-4","2009-1,5",
       "Created on 2009 1 6","2009 !!! 07")
ymd(x)

d <- "09/01/02"
ymd(d)
ydm(d)
myd(d)
mdy(d)
dym(d)
dmy(d)

Sys.time()#rbase function
now()#lubirdate function
olson_time_zones()

#text mining (case study : trump tweets)
library(dslabs)
data("trump_tweets")
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)
head(trump_tweets)
names(trump_tweets)
#The help file ?trump_tweets provides details on what each variable represents
trump_tweets %>% select(text) %>% head
# the source variable tells us the device that was used to compose and upload each tweet:
trump_tweets %>% count(source) %>% arrange(desc(n))
#We can use extract to remove the Twitter for part of the source and filter out retweets.
trump_tweets %>% extract(source, "source", "Twitter for (.*)") %>%
  count(source) 
#We are interested in what happened during the campaign, so for the analysis here 
#we will focus on what was tweeted between the day Trump announced his campaign and 
#election day
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)
#We can now use data visualization to explore the possibility that two different groups 
#were tweeting from these devices. For each tweet, we will extract the hour, 
#in the east coast (EST), it was tweeted then compute the proportion of tweets tweeted at each hour for each device.
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")
#Text as data
library(tidytext)
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

