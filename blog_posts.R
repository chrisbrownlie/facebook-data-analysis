devtools::install_github("chrisbrownlie/myFacebook")

library(myFacebook)
library(ggplot2)
library(ggimage)
library(ggmap)
library(leaflet)
library(formattable)
library(tidyr)
library(dplyr)
library(lubridate)
library(tidytext)
library(stringr)

# Post 1 - Usage of Facebook over time ------
posts_data <- get_posts()

posts <- gg_temporal(posts_data, 
                     aes(x = date_time)) + 
  geom_histogram(aes(x = date_time),
                 bins = 138,
                 fill = "#8b8fa6") +
  theme_brownlie() +
  labs(x = "Date",
       y = "No. of posts",
       title = "No. of Facebook posts over time")

posts

cb_save(posts, "posts_over_time")


# Comments
comms <- get_comments()

gg_comments <- gg_temporal(comms,
       aes(x = date_time)) +
  geom_histogram(aes(x = date_time),
                 bins = 138,
                 fill = "#8b8fa6") +
  theme_brownlie() +
  labs(x = "Date",
       y = "No. of comments",
       title = "No. of Facebook comments over time") +
  scale_y_continuous(breaks = c(0,20,40,60),
                     labels = c(0,20,40,60),
                     limits = c(0,70))

gg_comments

cb_save(gg_comments,
        "comments_over_time")


# Account activity
reactions <- get_reactions()
late_reactions <- reactions %>%
  filter(date_time >= ymd("2016-02-24")) %>%
  mutate(reaction = str_to_title(reaction))

gg_react <- gg_temporal(reactions,
            aes(x = date_time),
            label_height = 130) +
  geom_histogram(aes(x = date_time),
                 bins = 138,
                 fill = "#8b8fa6") +
  theme_brownlie() +
  lims(y = c(0,170)) +
  labs(x = "Date",
       y = "No. of reactions") +
  ggtitle("Number of reactions over time")
cb_save(gg_react,
        "reactions_over_time")

late_reactions$reaction <- factor(late_reactions$reaction, levels = c("Like", "Love", "Haha", "Wow", "Sorry", "Anger"))
gg_late_react <- ggplot(late_reactions,
            aes(x = date_time)) +
  geom_histogram(aes(x = date_time, fill = reaction),
                 bins = 57) +
  theme(axis.ticks.x = element_line(color = rep(c("black", NA), 11),
                                    size = rep(c(1.2, NA), 11))) +
  scale_x_datetime(breaks = seq(from = as.POSIXct("2016-01-01"),
                                to = as.POSIXct("2020-07-01"),
                                by = "6 month"),
                   labels = function(x) ifelse(month(x)==1, "", year(x))) +
  scale_fill_manual("Reaction",
                    breaks = c("Like",
                               "Love",
                               "Haha",
                               "Wow",
                               "Sorry",
                               "Anger"),
                    values = c("#3b5998",
                               "#c23a3a",
                               "#288f2b",
                               "#a6b541",
                               "#7c0085",
                               "#e89f31")) +
  labs(x = "Date", y = "No. of reactions") +
  ggtitle("Number of Facebook reactions of each type since 24th Feb 2016") +
  theme_brownlie()
cb_save(gg_late_react,
        "reactions_bytype_over_time")

gg_react_type <- reactions %>%
  filter(date_time > ymd("2016-02-24")) %>%
  count("reaction" = str_to_title(reaction)) %>%
  mutate(reaction = factor(reaction, levels = c("Like", "Love", "Haha", "Wow", "Anger", "Sorry"))) %>%
  ggplot(aes(x = reaction, fill = reaction)) +
  geom_col(aes(y = n), show.legend = FALSE) +
  geom_image(aes(y = 250, image = paste0("images/", tolower(reaction), "-transp.png"), size = 0.05)) +
  scale_size_identity() +
  geom_text(aes(y = n, label = n), vjust = -0.5) + 
  lims(y = c(0, 1900)) +
  theme_brownlie() +
  labs(x = "Type of reaction",
       y = "No. of times reaction used") +
  ggtitle("Types of reaction since 24th Feb 2016") +
  scale_fill_manual(breaks = c("Like",
                               "Love",
                               "Haha",
                               "Wow",
                               "Sorry",
                               "Anger"),
                    values = c("#3b5998",
                               "#c23a3a",
                               "#288f2b",
                               "#a6b541",
                               "#7c0085",
                               "#e89f31"))
cb_save(gg_react_type,
        "type_of_reaction_s2",
        w = 5,
        h = 5)


# Friends over time
fb_friends <- get_friends()

gg_friends <- gg_temporal(fb_friends,
            aes(x = added_time),
            label_height = 32) +
  geom_histogram(aes(x = added_time),
                 bins = 138,
                 fill = "#8b8fa6") +
  theme_brownlie() +
  labs(x = "Month",
       y = "No. of friends added") +
  ggtitle("Number of Facebook friends added over time") +
  scale_y_continuous(breaks = c(0,10,30,50),
                     labels = c(0,10,30,50),
                     limits = c(0,50))

cb_save(gg_friends,
        "friends_over_time")

# Post 2 - location and tracking data -----
profile_locations <- get_profile_information()
last_location <- get_last_location()
profile_update <- get_profile_update_history() %>%
  filter(!is.na(place_lat))

hometown <- convert_to_coordinates(profile_locations$hometown)
current_city <- convert_to_coordinates(profile_locations$current_city)

leaflet() %>%
  addTiles() %>%
  addCircles(lng = c(hometown[2], current_city[2]),
             lat = c(hometown[1], current_city[1]),
             radius = 5000,
             opacity = 0.1,
             fillOpacity = 0.4,
             weight = 1) %>%
  addCircles(lng = last_location$longitude,
             lat = last_location$latitude,
             radius = 3500,
             opacity = 0.1,
             fillOpacity = 0.4,
             weight = 1,
             fillColor = "red") %>%
  addLabelOnlyMarkers(lng = hometown[2],
                      lat = hometown[1]+0.04,
                      label = "Hometown, 'Primary Location' & 'Started School' (2007)",
                      labelOptions = labelOptions(noHide = T, direction = "top")) %>%
  addLabelOnlyMarkers(lng = current_city[2],
                      lat = current_city[1]+0.04,
                      label = "Current City, 'Started School' (2014) & 'Started New Job' (2017)",
                      labelOptions = labelOptions(noHide = T, direction = "top")) %>%
  addLabelOnlyMarkers(lng = last_location$longitude,
                      lat = last_location$latitude-0.02,
                      label = paste0("Last Location (", format.Date(last_location$date_time, "%d %b %Y"), ")"),
                      labelOptions = labelOptions(noHide = T, direction = "bottom"))


location_history <- get_location_history()

bitc <- filter(location_history, location == "Barton in the Clay") %>%
  slice(1) %>%
  mutate(label = "Where my parents live")
home <- filter(location_history, location == "Gamston") %>%
  slice(1) %>%
  mutate(label = "I live in Nottingham")
cbr <- filter(location_history, location == "Corbridge") %>%
  slice(1) %>%
  mutate(label = "Where my girlfriends parents live")
shef <- filter(location_history, location == "Sheffield") %>%
  slice(1) %>%
  mutate(label = "I often travel to Sheffield for work")
labels <- bind_rows(bitc, home, cbr, shef)

leaflet() %>%
  addTiles() %>%
  addCircles(lng = location_history$longitude,
             lat = location_history$latitude,
             radius = 10000,
             opacity = 0.1,
             fillOpacity = 0.1,
             weight = 1) %>%
  addLabelOnlyMarkers(lng = labels$longitude[-1],
                      lat = labels$latitude[-1]-0.05,
                      label = labels$label[-1],
                      labelOptions = labelOptions(noHide = T, direction = "bottom")) %>%
  addLabelOnlyMarkers(lng = labels$longitude[1]+0.05,
                      lat = labels$latitude[1],
                      label = labels$label[1],
                      labelOptions = labelOptions(noHide = T, direction = "right"))



account_activity <- get_account_activity()
account_unique <- account_activity %>%
  select(city, region) %>%
  unique()
account_activity_locations <- map2(.x = account_unique$city,
                                   .y = account_unique$region,
                                   .f = convert_to_coordinates)
walk(seq_len(length(account_activity_locations)), function(i) {
  if (is.logical(account_activity_locations[[i]])) {
    account_unique$latitude[i] <<- NA
    account_unique$longitude[i] <<- NA
  } else {
    account_unique$latitude[i] <<- account_activity_locations[[i]][[1]]
    account_unique$longitude[i] <<- account_activity_locations[[i]][[2]]
  }
})

account_activity_with_location <- account_activity %>%
  left_join(account_unique,
            by = c("city", "region"))

leaflet() %>%
  addTiles() %>%
  addCircles(lng = jitter(account_activity_with_location$longitude, amount = 0.1),
             lat = jitter(account_activity_with_location$latitude, amount = 0.1),
             fillOpacity = 0.3,
             opacity = 0.3,
             weight = 1,
             radius = 10000,
             label = paste0(account_activity_with_location$city, ", ", account_activity_with_location$region))

# Post 3 - likes and dislikes -----
ad_interests <- read.csv("ad_interests.csv") %>%
  transmute(interest = Interest,
            correct = Correct=="y",
            category = Category)

ordered_cats <- ad_interests %>%
  count(category) %>%
  arrange(desc(n)) %>% 
  pull(category)
ordered_ams <- ad_interests %>%
  count(category) %>%
  arrange(desc(n)) %>% 
  pull(n)

ad_interests$category <- factor(ad_interests$category, levels = c(ordered_cats[-4], "Other"))

cat_descs <- data.frame(Category = c(ordered_cats[-4], "Other"),
                        Description = c("Names of companies, terminology to do with business or technology",
                                        "Names of films/TV shows, genres etc.",
                                        "A person, real or fictional, alive or dead",
                                        "Country, city, state or establishment",
                                        "Sports or video games",
                                        "Cuisine, eating and drinking",
                                        "Political parties, organisations or terminology",
                                        "Theatre, specific cultures, media and history",
                                        "Bands, genres or music-related products",
                                        "Concepts, emotions and intangibles",
                                        "Subjects, education-related words",
                                        "Hobbies, holidays etc.",
                                        "Anything else"),
                        Examples = c("Instagram, Sales, Website",
                                     "Action movies, 8 out of 10 cats, Academy Awards",
                                     "Boris Johnson, Dwight Schrute, Woodrow Wilson",
                                     "Russia, Ljubljana, Zoo",
                                     "Rugby, Fortnite, Detroit Lions",
                                     "Legume, Nut, Dinner",
                                     "Activism, National Refugee Council, Elections",
                                     "Hispanic culture, Plays, Great Depression",
                                     "Alternative rock, Journey (Band), Ipod Touch",
                                     "Laughter, Solidarity, Reality",
                                     "Geometry, A-level, Probability",
                                     "Photography, Writing, Travel",
                                     "Ink, White (Colour), Week"),
                        Number = c(ordered_ams[-4], ordered_ams[4]),
                        stringsAsFactors = FALSE)

formattable(cat_descs,
            align = c("l", "l", "l", "r"),
            list(Number = color_bar("lightblue")))



recommended_topics <- read.csv("recommendations.csv") %>%
  pivot_longer(cols = c("watch", "feed", "news")) %>%
  filter(value) %>%
  transmute(topic = topic,
            interest = interest,
            category = Category,
            subcategory = subcategory,
            recommendation_type = case_when(
              name == "watch" ~ "Watch\nrecommendations",
              name == "news" ~ "News story\nrecommendations",
              name == "feed" ~ "Newsfeed\nrecommendations"
            ),
            all = 1)
recommended_topics$recommendation_type <- factor(recommended_topics$recommendation_type, levels = c("Watch\nrecommendations",
                                                                                                    "News story\nrecommendations",
                                                                                                    "Newsfeed\nrecommendations"))
ggplot(recommended_topics) +
  geom_bar(aes(x = all, fill = category), position = "fill") +
  facet_wrap(vars(recommendation_type)) +
  scale_fill_brewer("Topic Category", type="qual", palette=3) +
  labs(x = "",
       y = "Proportion of all recommended topics") +
  theme_brownlie() +
  theme(axis.text.x = element_text(color = NA),
        axis.ticks.x = element_line(color = NA)) +
  scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1),
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%"))



recommendations_2 <- recommended_topics %>%
  filter(category %in% c("Entertainment", "Food & Drink", "General", "Sport")) %>%
  mutate(interest_label = case_when(
    interest == 1 ~ "(1) No interest",
    interest == 2 ~ "(2) Little interest",
    interest == 3 ~ "(3) Some interest",
    interest == 4 ~ "(4) Interested",
    interest == 5 ~ "(5) Very interested"
  ))
recommendations_2$interest_label <- factor(recommendations_2$interest_label, levels = c("(1) No interest", 
                                                                                        "(2) Little interest", 
                                                                                        "(3) Some interest", 
                                                                                        "(4) Interested", 
                                                                                        "(5) Very interested"))
recommendations_2$category <- factor(recommendations_2$category, levels = c("Entertainment", "Sport", "Food & Drink", "General"))

ggplot(recommendations_2) +
  geom_bar(aes(x = category, fill = interest_label),
           position = "fill") +
  theme_brownlie() +
  labs(x = "Category",
       y = "Proportion of topics in category") +
  scale_fill_manual("Interest in topic",
                    values = c("(1) No interest" = "red",
                               "(2) Little interest" = "pink",
                               "(3) Some interest" = "orange",
                               "(4) Interested" = "light blue",
                               "(5) Very interested" = "light green")) +
  scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1),
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%")) +
  ggtitle("Level of interest in recommended topics of each category, across video, news and feed")

# Post 4 - off-Facebook activity -----
ads_contact_list <- get_advertisers_with_contact_info()
write.csv(ads_contact_list, "ads_contact_list.csv")

ads_contact_list <- read.csv("ads_contact_list.csv") %>%
  transmute(advertiser = Advertiser,
            known = Known=="y",
            expected = Expected=="y",
            category = Description)

sum(ads_contact_list$known)
sum(ads_contact_list$expected)

ad_interaction <- get_advert_interactions()



off_fb_act <- get_off_facebook_activity()
min(off_fb_act$date_time)
off_fb_act %>%
  group_by("Event" = type) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:6) %>%
  rename("No. of times recorded" = n) %>%
  formattable(align = c("l", "r"),
              list(`No. of times recorded` = color_bar("#78ff7a")))

off_fb_act %>%
  group_by("Website/App" = location) %>%
  summarise("Unique Event Types" = n_distinct(type),
            n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:8) %>%
  rename("No. of events sent to Facebook" = n) %>%
  formattable(align = c("l", "c", "r"),
              list(`No. of events sent to Facebook` = color_bar("#78ff7a")))


ofa_seconds <- off_fb_act %>%
  mutate(time = str_extract(date_time, pattern = "[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}"),
         seconds = as.numeric(str_extract(time, "[[:digit:]]{2}$")),
         mins = as.numeric(str_extract(time, "(?<=[[:digit:]]{2}:)[[:digit:]]{2}(?=:[[:digit:]]{2})")),
         hours = as.numeric(str_extract(time, "^[[:digit:]]{2}")),
         total_seconds = (hours*3600)+(mins*60)+seconds,
         group = floor(total_seconds/1800)) %>%
  count(group, location) %>%
  group_by(group) %>%
  summarise(total = sum(n),
            common_location = location[which.max(n)])
ggplot(ofa_seconds, aes(x = group, y = total)) +
  geom_col(aes(fill = common_location)) +
  theme_brownlie() +
  scale_x_continuous(breaks = seq(from = 0, to = 46, by = 4),
                     labels = c("", paste0(c(2,4,6,8,10,12), "am"), paste0(c(2,4,6,8,10), "pm"))) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = "Time of day",
       y = "Number of events shared with Facebook") +
  scale_fill_brewer("Most common location", type="qual", palette = 6) +
  ggtitle("Total number of events shared with Facebook by time of day",
          subtitle = "Colour represents most common location for each half hour")

# Post 5 - 4.5 years of a relationship in whatsapp activity -----
friend <- get_friends() %>%
  filter(friend_name == "Lizzy Parfitt")

comments <- get_comments() %>%
  filter(str_detect(comment, "Lizzy Parfitt")|str_detect(event_description, "Lizzy Parfitt"))

posts <- get_posts() %>%
  filter(str_detect(title, "Lizzy Parfitt")|str_detect(post, "Lizzy Parfitt"))

reactions <- get_reactions() %>%
  filter(reacted_to == "Lizzy Parfitt")

messages <- get_messages(participant = "Lizzy Parfitt")

words <- messages %>%
  mutate(content = tolower(gsub(content, pattern = "[^[:alnum:][:space:][:punct:]]", replacement = ""))) %>%
  unnest_tokens(output = "word", input = content, token = "words")
count(words, sender, word) %>%
  count(sender)


over_time <- bind_rows(transmute(comments, 
                                 date_time = date_time,
                                 type = "Comment"),
                       transmute(posts,
                                 date_time = date_time,
                                 type = "Status"),
                       transmute(reactions,
                                 date_time = date_time,
                                 type = "Reaction"))
ggplot(over_time, aes(x = date_time)) +
  geom_histogram(aes(fill = type), binwidth = 60*60*24*28) +
  theme_brownlie() +
  scale_fill_brewer("Interaction type", type = "qual", palette = 3) +
  labs(x = "Date",
       y = "Total no. of interactions") +
  ggtitle("Facebook interactions between my girlfriend & I each month") +
  scale_x_datetime(breaks = as.POSIXct(seq.Date(from = ymd("2016-01-01"), to = ymd("2021-01-01"), by = "6 month")),
                   labels = function(x) {
                     ifelse(month(x)==1,
                            "",
                            format.Date(x, format = "%Y"))
                   }) +
  theme(axis.ticks.x = element_line(size = rep(c(0.5,NA),5)),
        axis.ticks.length.x = unit(0.3, "cm"),
        axis.text.x = element_text(vjust = 7))


# Messages over time
messages$sender[messages$sender == "Chris Brownlie"] <- "Me"
messages$sender[messages$sender == "Lizzy Parfitt"] <- "Her"

messages_annotations <- data.frame(x = c(as.POSIXct(ymd("2016-11-01")),
                                         as.POSIXct(ymd("2018-06-01"))),
                                   xend = c(as.POSIXct(ymd("2016-03-20")),
                                            as.POSIXct(ymd("2017-11-01"))),
                                   y = c(3500, 1900),
                                   yend = c(2400, 700),
                                   label = c("62% of messages\nin the month and a half\nbefore we started dating",
                                             "13.7% of messages\nin the month after\nI started my graduate job"),
                                   stringsAsFactors = FALSE)

ggplot(messages, aes(x = date_time)) +
  geom_histogram(aes(fill = sender), binwidth = 60*60*24*28) +
  theme_brownlie() +
  labs(x = "Date",
       y = "Total no. of messages") +
  ggtitle("Facebook messages between my girlfriend & I each month") +
  scale_x_datetime(breaks = as.POSIXct(seq.Date(from = ymd("2015-12-01"), to = ymd("2020-12-01"), by = "6 month")),
                   labels = function(x) {
                     ifelse(month(x)==12,
                            "",
                            format.Date(x, format = "%Y"))
                   },
                   limits = c(min(messages$date_time), as.POSIXct(ymd("2020-07-01")))) +
  scale_fill_manual("Sender",values = c("Me" = "lightblue",
                               "Her" = "pink")) +
  theme(axis.ticks.x = element_line(size = rep(c(0.5,NA),5)),
        axis.ticks.length.x = unit(0.3, "cm"),
        axis.text.x = element_text(vjust = 6),
        text = element_text(size = 16)) +
  geom_segment(data = messages_annotations,
             aes(x = x,
                 xend = xend,
                 y = y,
                 yend = yend),
             arrow = arrow(length = unit(0.25, "inches"))) +
  geom_text(data = messages_annotations,
            aes(x = x + months(6),
                y = y+10,
                label = label))

# Reactions by type
reaction_types <- reactions %>%
  count(reaction)

lizzy_reactions <- reactions %>%
  count("reaction" = str_to_title(reaction)) %>%
  mutate(reaction = factor(reaction, levels = c("Like", "Love", "Haha", "Sorry", "Wow", "Anger"))) %>%
  ggplot(aes(x = reaction, fill = reaction)) +
  geom_col(aes(y = n), show.legend = FALSE) +
  geom_image(aes(y = 30, image = paste0("images/", tolower(reaction), "-transp.png"), size = 0.05)) +
  scale_size_identity() +
  geom_text(aes(y = n, label = n), vjust = -0.5) + 
  lims(y = c(0, 200)) +
  theme_brownlie() +
  labs(x = "Type of reaction",
       y = "No. of times reaction used") +
  ggtitle("Types of reaction I've had to my girlfriend on Facebook") +
  scale_fill_manual(breaks = c("Like",
                               "Love",
                               "Haha",
                               "Wow",
                               "Sorry",
                               "Anger"),
                    values = c("#3b5998",
                               "#c23a3a",
                               "#288f2b",
                               "#a6b541",
                               "#7c0085",
                               "#e89f31"))
cb_save(lizzy_reactions, "lizzy_reactions_type", w = 7, h = 7)

reactions$content[reactions$content == " post"] <- " status"
reactions %>%
  mutate("Content I 'reacted' to" = str_to_title(trimws(content))) %>%
  ggplot(aes(x = date_time)) +
  geom_histogram(aes(fill = `Content I 'reacted' to`), binwidth = 60*60*24*28) +
  theme_brownlie() +
  labs(x = "Date",
       y = "No. of times I 'reacted'") +
  ggtitle("My reactions to my girlfriend's content on Facebook each month") +
  scale_x_datetime(breaks = as.POSIXct(seq.Date(from = ymd("2015-12-01"), to = ymd("2020-12-01"), by = "6 month")),
                   labels = function(x) {
                     ifelse(month(x)==12,
                            "",
                            format.Date(x, format = "%Y"))
                   },
                   limits = c(min(messages$date_time), as.POSIXct(ymd("2020-07-01")))) +
  theme(axis.ticks.x = element_line(size = rep(c(0.5,NA),5)),
        axis.ticks.length.x = unit(0.3, "cm"),
        axis.text.x = element_text(vjust = 6),
        text = element_text(size = 16))


# Message content
liz <- words %>%
  ungroup() %>%
  filter(sender=="Lizzy Parfitt",
         !word %in% c(stop_words$word, "nala", "ð"),
         !is.na(word)) %>%
  count(word) %>%
  top_n(10, wt = n) %>%
  arrange(desc(n)) %>%
  slice(1:10)
liz$word <- ordered(liz$word, levels = liz$word[order(liz$n)])

gg_liz <- ggplot(liz, aes(x = word, y = n)) +
  geom_bar(fill = "pink", show.legend = FALSE, stat = "identity") +
  geom_text(aes(label = n, y = (n+20))) +
  coord_flip() +
  ggthemes::theme_hc() +
  scale_fill_brewer(type = "qual") +
  labs(x = "", y = "") + 
  theme(rect = element_rect(fill = "transparent"),
        line = element_line(colour = "black"),
        axis.text.y = element_text(color = "black", size = 18),
        axis.text.x = element_text(color = "white"),
        axis.ticks = element_line(size = NA),
        panel.grid.minor.y = element_line(size = NA),
        panel.grid.major.y = element_line(size = NA),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Her")

me <- words %>%
  ungroup() %>%
  filter(sender=="Chris Brownlie",
         !word %in% c(stop_words$word, "ð", "ó", "ill"),
         !is.na(word)) %>%
  count(word) %>%
  top_n(10, wt = n)
me$word <- ordered(me$word, levels = me$word[order(me$n)])

gg_me <- ggplot(me, aes(x = word, y = n)) +
  geom_bar(fill = "lightblue", show.legend = FALSE, stat = "identity") +
  geom_text(aes(label = n, y = (n+20))) +
  coord_flip() +
  ggthemes::theme_hc() +
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  scale_fill_brewer(type = "qual") +
  labs(x = "", y = "") + 
  theme(line = element_line(colour = "black"),
        axis.text = element_text(color = "black", size = 18),
        axis.text.x = element_text(color = "white"),
        axis.ticks = element_line(size = NA),
        panel.grid.minor.y = element_line(size = NA),
        panel.grid.major.y = element_line(size = NA),
        plot.title = element_text(hjust = 0.55)) +
  ggtitle("Me")

library(grid)
library(gridExtra)
words_plots <- grid.arrange(gg_me, gg_liz, ncol = 2)
