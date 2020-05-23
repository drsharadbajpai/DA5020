#Warm-up
farmers_market_csv$City_State <- paste(farmers_market_csv$city,farmers_market_csv$State, sep = ", ")

Question1:
  
library(tidyverse)
library(readxl)
library(stringr)

# let `read_csv` handle special N/A values
na.vals <- c("", "NA", "n/a", "N/A", "none")
farmers_market_csv <- read_csv("C:/Users/bajpa/Downloads/Collecting, Storing, Retrieving Data/Week4/farmers_market.csv.csv")
library(readr)
kyfprojects_xls <- read_csv("C:/Users/bajpa/Downloads/Collecting, Storing, Retrieving Data/Week4/kyfprojects.xls.xls")
View(kyfprojects_xls, na = na.vals)
re_facebook <- str_c(
  "(?i)",
  "^(?:.*(?:f?acebook|fb)(?:\\.com)?[/ ]?",
  "(?:#\\!/)?(?:pages/)?)?",
  "(?:@)?",  
  "([^#]*?)",
  "/?",           
  "(?:\\?.*)?$"
)


re_twitter <- str_c(
  "(?i)",
  "(?:.*[\\@/])?([a-z0-9_]+)"
)

# Cleaning Facebook and Twitter column


fmarkets <- farmers_market_csv %>%
  mutate(
    Facebook.clean = Facebook %>%
      str_replace(re_facebook, "\\1"),
    Facebook.clean = ifelse(Facebook.clean == "", NA, Facebook.clean),
    
    Twitter.clean = Twitter %>%
      str_replace(re_twitter, "\\1"),
    
    Twitter.clean = ifelse(Twitter.clean == "", NA, Twitter.clean)
  )

#Questions #2

street <- mutate(fmarkets, street=str_replace_all(street,"[ ]ST[ ]|[Ss]treet|ST[.]St[.]|streets|StreetsStreets|[ ]ST[ ]|ST", replace="St"))

and <- mutate(street, street=str_replace_all(street, "[ ] and [ ]" , replace = "&"))

street_mutated <- mutate(and, city=str_replace_all(city, "[,][ ]+[A-Z][A-Z]", replace=""))

#Cleaning city column
q2<-mutate(and, city=str_replace_all(city,"[,][ ][A-Z][A-Z]+|[ ]IN|California|Texas|Idaho|MA|Florida|NM|[ ]ND",replace=""))
city_mutated<-mutate(q2,city=str_replace_all(city,"[,][ ]Ma[.]",replace=""))

head(city_mutated$street, 20L)
View(city_mutated)


#Question #3
online_persence <- fmarkets %>% 
  group_by(fmarkets$State) %>% 
  summarise(
    Website_presence = (sum(!is.na(Website))/n()*100),
    Facebook_presence = (sum(!is.na(Facebook.clean))/n()*100),
    Twitter_presence = (sum(!is.na(Twitter.clean))/n()*100),
    Youtube_presence = (sum(!is.na(Youtube))/n()*100),
    Othermedia_prop = (sum(!is.na(OtherMedia))/n()*100)
  )
online_pres <- as.tibble(online_persence)
head(online_pres, 20L)
view(online_pres)

#Question #4
library(forcats)

q4 <- fmarkets %>% 
  group_by(Location) %>% 
  summarise(count=n()) %>% 
  mutate(Location = fct_recode(Location,
                                "Private_PL" = "Private business parking lot",
                                "Local gov" = "Local government building grounds",
                                "Edu Inst" = "Educational institution",
                                "Clo Pub St" = "Closed-off public street",
                                "Fed blg gr" = "Federal/State government building grounds",
                                "Other" = "Other",
                                "Hc Inst" = "Healthcare Institution",
                                "On farm" = "On a farm from: a barn, a greenhouse, a tent, a stand, etc",
                                "Faith Inst" = "Faith-based institution (e.g., church, mosque, synagogue, temple)",
                                "Co w mr fclty" = "Co-located with wholesale market facility"))
#Plot
ggplot(data=q4, aes(y = reorder(q4$Location, q4$count) , x = q4$count)) +
  geom_point(color="red")+
  xlab("Count")+
  ylab("Location")

#Question #5

ggplot(data = q4, mapping = aes(x = reorder(q4$Location, q4$count), y = q4$count)) +
  geom_bar(stat = "identity") + 
  coord_flip() + ylim(0, 1000) + 
  title = "Locations of Farmer Markets" +
xlab = "Location" + ylab = "Count"

------------
