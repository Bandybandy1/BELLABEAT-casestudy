install.packages("rmarkdown")
install.packages("here")
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("lubridate")
install.packages("ggpubr")
install.packages("ggrepel")
install.packages("ggplot2")

library(ggpubr)
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(readr)

hourly_steps <- read_csv("hourlySteps_merged.csv")
View(hourly_steps)


daily_activity <- read_csv("Daily_Activity_Merged.csv")
View(daily_activity)

daily_sleep <- read_csv("sleepDay_merged.csv")
View(daily_sleep)
View(daily_sleep)

head(daily_activity)
str(daily_activity)

head(daily_sleep)
str(daily_sleep)

head(hourly_steps)
str(hourly_steps)

#look how many unique users are pero dataframe 

n_unique(daily_activity$Id)
n_unique(daily_sleep$Id)
n_unique(hourly_steps$Id)

sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_steps))

#to remove duplicates 
daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()

daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()

hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()

sum(duplicated(daily_sleep))

#change the format to lower case

clean_names(daily_activity)
daily_activity <- rename_with(daily_activity, tolower)

clean_names(daily_sleep)
daily_sleep <- rename_with(daily_sleep, tolower)

clean_names(hourly_steps)
hourly_steps <- rename_with(hourly_steps, tolower)

#clean date-time format

daily_activity <- daily_activity %>% 
  rename(date = activitydate) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))


daily_sleep <- daily_sleep %>% 
  rename(date = sleepday) %>% 
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p" ,
  tz=Sys.timezone()))


head(daily_activity)
head(daily_sleep)

hourly_steps <- hourly_steps %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct (date_time,
  format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone())) 

head(hourly_steps)

#merge data of daily activity and daily sleep

daily_activity_sleep <- merge(daily_activity, daily_sleep, by = c("id", "date"))
glimpse(daily_activity_sleep)

daily_average <- daily_activity_sleep %>% 
  group_by(id) %>%
  
  summarise (mean_daily_steps = mean(totalsteps),
             mean(calories), mean_daily_sleep = (totalminutesasleep))

head(daily_average)

  user_type <- daily_average %>% 
    mutate(user_type = case_when (
      mean_daily_steps < 5000 ~ "sedentary",
      mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ "lightly active", 
      mean_daily_steps >= 7500 & mean_daily_steps < 9999 ~ "fairly active", 
      mean_daily_steps >= 10000 ~ "very active"
    ))
  
  head(user_type)
  
  str(user_type)
  
  #Now that we have a new column with the user type we
  #will create a data frame with the percentage of each user type to better visualize them on a graph.
  
  user_type_percent <- user_type %>% 
    
    group_by(user_type) %>% 
    summarise( total = n ()) %>% 
    mutate(totals = sum(total)) %>% 
    group_by(user_type) %>% 
    summarise (total_percent = total / totals) %>% 
    mutate(labels = scales :: percent(total_percent))
  
  user_type_percent$user_type <- factor(user_type_percent$user_type, levels = 
                   c("very active", "fairly active","lightly active", "sedentary")) 
  
  head(user_type_percent)
  View(user_type_percent)

  user_type_percent %>%
    ggplot(aes(y = "total_percent", x="user_Type")) +
    geom_density(alpha = 0.3)+
    xlim(55, 70)

labs(title="User type distribution")

  user_type_percent %>% 
    ggplot(aes(x="total_percent",y= , color =user_type )) +
    geom_density(alpha = 0.1) +
    xlim(55, 70)
    ggplot(aes(x="", y=total_percent, fill=user_type)) + 
    geombar(stat= "identity",width= 1)+ 
    coord_polar("y", start=0)+
      
    labs(title="User type distribution")
  
  
  weekday_steps_sleep <- daily_activity_sleep %>%
    mutate(weekday = weekdays(date))
  
  weekday_steps_sleep$weekday <-ordered(weekday_steps_sleep$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                              "Friday", "Saturday", "Sunday"))
  weekday_steps_sleep <-weekday_steps_sleep%>%
    group_by(weekday) %>%
    summarize (daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep))
  
  head(weekday_steps_sleep)
    
#bar charts: 
  ggarrange(
    ggplot(weekday_steps_sleep) +
      geom_col(aes(weekday, daily_steps), fill = "#006699") +
      geom_hline(yintercept = 7500) +
      labs(title = "Daily steps per weekday", x= "", y = "") +
      theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)),
    
    ggplot(weekday_steps_sleep, aes(weekday, daily_sleep)) +
      geom_col(fill = "#85e0e0") +
      geom_hline(yintercept = 480) +
      labs(title = "Minutes asleep per weekday", x= "", y = "") +
      theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
  )

  hourly_steps <- hourly_steps %>%
    separate(date_time, into = c("date", "time"), sep= " ") %>%
    mutate(date = ymd(date)) 

  head(hourly_steps)

#geomline with wrong data
  
ggplot(daily_activity_sleep, aes(x=totalsteps, y=totalminutesasleep,color=totalminutesasleep))+
      geom_area( fill="#69b3a2", alpha=0.4) +
      geom_line(color="#69b3a2") +
      geom_point(color="#69b3a2")+
      labs(title = "Daily steps vs Minutes asleep", x = "Daily steps", y= "Minutes asleep") +
      theme(panel.background = element_blank(),
            plot.title = element_text( size=14))
#first plot to daily steps and calories burnt

   ggplot(daily_activity_sleep, aes(x=totalsteps, y=calories,color=calories))+
    geom_jitter() +
    geom_smooth(color = "red", se = FALSE) + 
    labs(title = "Daily steps vs Calories", x = "Daily steps", y= "Calories")+
   scale_color_gradient(low = "yellow", high = "red")+
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14 ,hjust = 0.5, face="bold"))
   
   #second graph in scaterrplot to verify correlation bewteen minutes asleep and daily steps
   
   ggplot(daily_activity_sleep, aes(x=totalsteps, y=totalminutesasleep,color=totalminutesasleep))+
     geom_jitter() +
     geom_smooth(color = "blue", se = FALSE) + 
     labs(title = "Daily steps vs Minutes asleep", x = "Daily steps", y= "Minutes asleep") +
     theme(panel.background = element_blank(),
           plot.title = element_text( size=14 ,hjust = 0.5, face="bold"))
   ### **5. Analyze Phase and Share Phase  **
   
#Lets find some trends between data frames to determine this information is going to be useful to resolve our business task and of there is some approach insights we can get the BellaBeats marketing Strategy.
   
   
   ### **5.1 Correlations**
#We are going to verify some correlations between Calories and daily steps
     
     ```{plot}
   ggplot(daily_activity_sleep, aes(x=totalsteps, y=calories,color=calories))+
     geom_jitter() +
     geom_smooth(color = "red", se = FALSE) + 
     labs(title = "Daily steps & Calories", x = "Daily steps", y= "Calories")+
     scale_color_gradient(low = "yellow", high = "red")+
     theme(panel.background = element_blank(),
           plot.title = element_text( size=14, hjust = 0.5, face="bold"))
   ```
#We can clearly see a correlation between daily steps and calories, the more steps made the more calories burned. We can focus on customers that doesn`t achieve the goal on the daily average steps and encourage them.

#Now lets find the correlation between minutes asleep and daily steps

```{scatterplot}
   ggplot(daily_activity_sleep, aes(x=totaltimeinbed, y=totalminutesasleep,color=totalminutesasleep))+
     geom_jitter() +
     geom_smooth(color = "blue", se = FALSE) + 
     labs(title = "Total Time in bed & Minutes asleep", x = "Time in Bed", y= "Minutes Asleep") +
     theme(panel.background = element_blank(),
           plot.title = element_text( size=14, hjust = 0.5, face="bold"))

view(daily_activity_sleep)

```

#Based on our plots we can sees users sleep a day.
#we can see a positive correlation between total time in bed and minutes asleep, this can help us focus on users that spend more time in bed and encourage them to sleep the average hours to have better rest. 

###**5.1 Define type of users**

#We want to determine the type of users with the data we have. We can classify the users by activity considering the daily amount of steps. We can categorize users as follows:

# + Sedentary 
# + Lightly active 
# + Fairly active 
# + Very active 

#First we will calculate the daily steps average by user compare to the daily average steps in general.

```{merging_datasets}
daily_average <- daily_activity_sleep %>%
  group_by(id) %>%
  summarise (mean_daily_steps = mean(totalsteps), mean_daily_calories = mean(calories), mean_daily_sleep = mean(totalminutesasleep))


head(daily_average)

```

###Now we will classify the users in the 4 categories. 


```{user_type}
user_type <- daily_average %>%
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ "sedentary",
    mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ "lightly active", 
    mean_daily_steps >= 7500 & mean_daily_steps < 9999 ~ "fairly active", 
    mean_daily_steps >= 10000 ~ "very active"
  ))
head(user_type)
View(user_type)
```

###Now we can to plot the average user type to analize trends or which category is the most commmon in between them.


```{average_user_type}
 user_type %>% 
ggplot(aes(mean_daily_calories,color=user_type,fill=user_type)) +
    geom_density(alpha = 0.1) + 
    labs(title = "User Type", x = "Calories", y= "Users")+
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14, hjust = 0.5, face="bold"))


```
#we  can see that users are fairly distributed by their activity considering the daily amount of calories burned. We can determine that even if most of the users are fairly distributed on these 4 categories the sedentary ones can have some notifications to encourage them and use the device more.

### **5.2 Steps & Weekdays **

```{week days}
weekday_steps_sleep <- daily_activity_sleep %>%
  mutate(weekday = weekdays(date))

weekday_steps_sleep$weekday <-ordered(weekday_steps_sleep$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
"Friday", "Saturday", "Sunday"))

 weekday_steps_sleep <-weekday_steps_sleep%>%
  group_by(weekday) %>%
  summarize (daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep))

head(weekday_steps_sleep)
```

    ggplot(weekday_steps_sleep) +
      geom_col(aes(weekday, daily_steps), fill = "light blue") +
      geom_hline(yintercept = 7500, color = "red") +
      labs(title = "Daily Steps per weekday", x= "", y = "Daily steps") +
      theme(axis.text.x = element_text(hjust = 1), plot.title = element_text( size=14, hjust = 0.5, face="bold"), panel.background = element_blank())



#In the graphs above we can determine the following

#Users walk daily the recommended amount of steps of 7500 besides Sundays.
   
   
   
   
