source("./crime_library.R")
require(ggplot2)

## What are the five most common crime types (aggregated across neighborhoods and hours), 
## and how many of each such crime occurred? Be alert for misspellings!
Most_Common_Types <- all_crimes %>%
  group_by(Crime_Type) %>%
  summarise(Crime_Type_Num = n()) %>%
  arrange(desc(Crime_Type_Num)) %>%
  slice(1:5)

Most_Common_Types

## Make a plot of the total number of crimes (aggregated across neighborhoods and
## crime types) by hour. Write a few sentences about the pattern you observe.
theme_set(theme_bw())

p <- ggplot(data = all_crimes)
p + 
  geom_bar(aes(x=factor(Hour))) +
  xlab("Hour of Day") +
  ylab("Crime Count (Aggregated across neighborhoods and crime types)")


## Restrict to the five most common crime types, and plot the total number of crimes
## (aggregated across neighborhoods) for each crime type by hour (i.e., your plot should
## have five lines). Write a few sentences about the pattern you observe.

# Stacked Bar Plot (I think looks better)
q <- ggplot(data = all_crimes %>%
              filter(Crime_Type %in% Most_Common_Types$Crime_Type))
q + 
  geom_bar(aes(x=factor(Hour), fill = Crime_Type)) +
  xlab("Hour of Day") +
  ylab("Crime Count (Aggregated across neighborhoods)")

# Line Plot
q <- ggplot(data = all_crimes %>%
              filter(Crime_Type %in% Most_Common_Types$Crime_Type) %>%
              group_by(Hour,Crime_Type) %>%
              summarise(Crime_Count = n()))

q +
  geom_line(aes(x = Hour, y = Crime_Count, color = Crime_Type)) +
  xlab("Hour of Day") +
  ylab("Crime Count (Aggregated across neighborhoods)")


## Restrict to just the neighborhoods of Dorchester and Downtown, and plot the total
## number of crimes (aggregated across crime types (include all crime types, not just the
## top five)) for each of the two neighborhoods by hour (i.e., your plot should have two
## lines). Write a few sentences about the pattern you observe.

# Stacked Bar Plot (I think looks better)
g <- ggplot(data = all_crimes %>%
              filter(Neighborhood %in% c("dorchester","downtown")))

g +
  geom_bar(aes(x=factor(Hour), fill = Neighborhood)) +
  xlab("Hour of Day") +
  ylab("Crime Count (Aggregated across crime types)")

# Line Plot
g <- ggplot(data = all_crimes %>%
              filter(Neighborhood %in% c("dorchester","downtown")) %>%
              group_by(Hour, Neighborhood) %>%
              summarise(Crime_Count = n()))

g +
  geom_line(aes(x = Hour, y = Crime_Count,color = Neighborhood)) +
  xlab("Hour of Day") +
  ylab("Crime Count (Aggregated across crime types)")


