library(data.table)
library(dtplyr)
library(dplyr)
library(httr)

options(timeout = 300)

LAdata <- data.table::fread("C:/Users/ellya/OneDrive/Desktop/Final Project/Final Project/Crime_Data_from_2010_to_2019_20241025.csv")

str(LAdata)
#Important variables to look at: Vict Age, Vict Sex, Vic Descent, Crm Cd, LAT, LON, AREA NAME

dim(LAdata)
sum(duplicated(LAdata)) #checked for dulplicated data --> no duplicates

#Victim Age
#An age of zero appeared to have been used when age was unknown, making it challenging to differentiate between crimes involving children of 0 years old and those where the age was not recorded. Consequently, all observations with an age of 0 were converted into missing values, except for those associated with crime codes related to children. 
table(LAdata$`Vict Age`)
LAdata$`Vict Age`[LAdata$`Vict Age` < 0] <- NA
LAdata$`Vict Age`[LAdata$`Vict Age` == 0 & LAdata$`Crm Cd Desc` %in% c(
  "CHILD ABANDONMENT", 
  "CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT", 
  "CHILD ABUSE (PHYSICAL) - AGGRAVATED ASSAULT", 
  "CHILD ANNOYING (17YRS & UNDER)", 
  "CHILD NEGLECT (SEE 300 W.I.C.)", 
  "CHILD PORNOGRAPHY", 
  "CHILD STEALING", 
  "LEWD/LASCIVIOUS ACTS WITH CHILD"
)] <- 0

LAdata$`Vict Age`[LAdata$`Vict Age` == 0 & !LAdata$`Crm Cd Desc` %in% c(
  "CHILD ABANDONMENT", 
  "CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT", 
  "CHILD ABUSE (PHYSICAL) - AGGRAVATED ASSAULT", 
  "CHILD ANNOYING (17YRS & UNDER)", 
  "CHILD NEGLECT (SEE 300 W.I.C.)", 
  "CHILD PORNOGRAPHY", 
  "CHILD STEALING", 
  "LEWD/LASCIVIOUS ACTS WITH CHILD"
)] <- NA

table(LAdata$`Vict Age`)
sum(is.na(LAdata$`Vict Age`))
hist((LAdata$`Vict Age`))

#Victim Sex
#	F - Female M - Male X - Unknown
table(LAdata$`Vict Sex`)
LAdata <- LAdata |>
  mutate(`Vict Sex` = case_when(
    `Vict Sex` %in% c("H", "N", "-", "") ~ "X",
    TRUE ~ `Vict Sex`))
table(LAdata$`Vict Sex`)
sum(is.na(LAdata$`Vict Sex`))
barplot(table(LAdata$`Vict Sex`))

#Victim Descent --> summarized into 6 races
sum(is.na(LAdata$`Vict Descent`))
LAdata <- LAdata |>
  mutate(`Vict Descent` = case_when(
    `Vict Descent` %in% c("-", "") ~ "X",
    `Vict Descent` %in% c("C", "D", "F", "J", "K", "L", "V", "Z") ~ "A",
    `Vict Descent` %in% c("G", "S", "U", "O") ~ "P",
    TRUE ~ `Vict Descent`))
LAdata <- LAdata[LAdata$`Vict Descent` %in% c("A", "B", "H", "W", "P", "X")]
barplot(table(LAdata$`Vict Descent`))

#Crime Code
table(LAdata$`Crm Cd Desc`)
sum(is.na(LAdata$`Crm Cd Desc`))

#LAT, LON, and AREA
#It was noted that unknown LAT and LON data was listed as 0.
LAdata$'LAT'[LAdata$'LAT' == 0] <- NA
LAdata$'LON'[LAdata$'LON' == 0] <- NA
sum(is.na(LAdata$`LON`))
sum(is.na(LAdata$`LAT`))
table(LAdata$`AREA NAME`)
sum(is.na(LAdata$`AREA NAME`))
sum(!is.na(LAdata$`AREA NAME`))
barplot(table(LAdata$`AREA NAME`))

library(ggplot2)
library(RColorBrewer)
library(stringr)
library(forcats)
library(data.table)

LAdatanomissing <- LAdata |>
  filter(!is.na(`Vict Sex`) &
           !is.na(`Vict Descent`) &
           !is.na(`Vict Age`) &
           !is.na(`LAT`) & 
           !is.na(`LON`)
  )
dim(LAdatanomissing)

#Race and gender for LA
LAdatanomissing |>
  ggplot() +
  geom_bar(mapping = aes(x = `Vict Descent`, fill = `Vict Sex`), position = "dodge") +
  labs(title = "Number of Crimes Categorized by Race and Gender") +
  labs(x = "Race", y = "Number of Crimes Experienced", fill = "Gender") +
  scale_fill_manual(values = c("F" = "pink", "M" = "lightblue", "X" = "gray"),
                    labels = c("F" = "Female", "M" = "Male", "X" = "Unknown")) +
  scale_x_discrete(labels = c(
    "A" = "Asian",
    "B" = "Black",
    "H" = "Hispanic",
    "P" = "Pacific Islander",
    "W" = "White",
    "X" = "Unknown"
  ))

crimesbyrace <- LAdatanomissing |>
  group_by(`Vict Sex`, `Vict Descent`) %>%
  summarise(
    Entries = n(),
    .group = 'drop'
  ) 
crimesbyrace <- crimesbyrace[order(crimesbyrace$Entries, decreasing = TRUE),]
crimesbyrace

#LA age distribution

custom_title1 <- c("Number of Crimes Reported by Age in Los Angeles from 2010-2019") 

wrapped_title1 <- sapply(custom_title1, function(x) str_wrap(x, width = 50))

LAage <- LAdatanomissing |>
  mutate(AgeGroup = cut(`Vict Age`, breaks = seq(0, 120, by = 10), right = FALSE, 
                        labels = paste(seq(0, 110, by = 10), seq(9, 119, by = 10), sep = "-")))

plotLAage <- LAage |>
  ggplot(aes(x = `AgeGroup`, fill = `AgeGroup`)) +
  geom_bar(position = "dodge") +
  labs(title = wrapped_title1) +
  labs(x = "Age (Years)", y = "Number of Crimes Reported") +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plotLAage

#Hispanic Female Age Distribution

custom_title2 <- c("Number of Crimes Reported by Hispanic Females by Age in Los Angeles from 2010-2019") 

wrapped_title2 <- sapply(custom_title2, function(x) str_wrap(x, width = 50))

HispanicMales <- LAdatanomissing[ `Vict Sex` == "M" & `Vict Descent` == "H"]
HispanicFemales <- LAdatanomissing[ `Vict Sex` == "F" & `Vict Descent` == "H"]
HispanicFemales[, AgeGroup := cut(`Vict Age`, breaks = seq(0, 120, by = 10), 
                                  right = FALSE, 
                                  labels = paste(seq(0, 110, by = 10), seq(9, 119, by = 10), sep = "-"))]

plotHFage <- HispanicFemales |>
  ggplot(aes(x = `AgeGroup`, fill = `AgeGroup`)) +
  geom_bar(position = "dodge") +
  labs(title = wrapped_title2) +
  labs(x = "Age (Years)", y = "Number of Crimes Reported") +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plotHFage

#Top 5 crimes in Los Angeles
LAcrimes <- LAdatanomissing |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
LAcrimes

Females <- LAdatanomissing[`Vict Sex` == "F"]
Males <- LAdatanomissing[`Vict Sex` == "M"]

#Top 5 crimes done to males
Malecrimes <- Males |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
Malecrimes

#Top 5 crimes done to females
Femalecrimes <- Females |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
Femalecrimes

#Top 5 Crimes reported by hispanic men 
HispanicMalecrimes <- HispanicMales |>
  filter(!is.na(`Crm Cd Desc`) &
           !is.na(`Vict Sex`) &
           !is.na(`Vict Descent`)) |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
HispanicMalecrimes

#Top 5 Crimes reported by hispanic women 
HispanicFemalecrimes <- HispanicFemales |>
  filter(!is.na(`Crm Cd Desc`) &
           !is.na(`Vict Sex`) &
           !is.na(`Vict Descent`)) |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
HispanicFemalecrimes

#Top 5 Crimes reported by hispanic women between 20 and 29
HispanicFemalecrimesAge <- HispanicFemales |>
  filter(!is.na(`Crm Cd Desc`) &
           !is.na(`Vict Sex`) &
           !is.na(`Vict Descent`) &
           AgeGroup == "20-29") |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
HispanicFemalecrimesAge

custom_label <- c(
  "BATTERY - SIMPLE ASSAULT" = "Battery - Simple Assault",
  "BURGLARY FROM VEHICLE" = "Burglary from Vehicle",
  "INTIMATE PARTNER - SIMPLE ASSAULT" = "Intimate Partner - Simple Assault",
  "THEFT PLAIN - PETTY ($950 & UNDER)" = "Petty Theft (< $950",
  "VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)" = "Vandalism (> $400)"
) 

wrapped_labels <- sapply(custom_label, function(x) str_wrap(x, width = 15))

custom_title3 <- c("Top 5 Crimes Experienced by Hispanic Females Aged 20-29 in Los Angeles from 2010-2019")
wrapped_title3 <- sapply(custom_title3, function(x) str_wrap(x, width = 50))

HispanicFemalecrimesAge <- HispanicFemalecrimesAge %>%
  mutate(`Crm Cd Desc` = fct_reorder(`Crm Cd Desc`, Entries, .desc = TRUE))

ggplot(data = HispanicFemalecrimesAge, aes(x = `Crm Cd Desc`, y = Entries, fill = `Crm Cd Desc`)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues", labels = custom_label) +
  labs(title = wrapped_title3) +
  labs(x = "Type of Crime", y = "Number of Crimes Reported") +
  scale_x_discrete(labels = wrapped_labels) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)
library(RColorBrewer)
library(stringr)
library(forcats)
library(data.table)

LAdatanomissing <- LAdata |>
  filter(!is.na(`Vict Sex`) &
           !is.na(`Vict Descent`) &
           !is.na(`Vict Age`) &
           !is.na(`LAT`) & 
           !is.na(`LON`)
  )
dim(LAdatanomissing)

#Race and gender for LA
LAdatanomissing |>
  ggplot() +
  geom_bar(mapping = aes(x = `Vict Descent`, fill = `Vict Sex`), position = "dodge") +
  labs(title = "Number of Crimes Categorized by Race and Gender") +
  labs(x = "Race", y = "Number of Crimes Experienced", fill = "Gender") +
  scale_fill_manual(values = c("F" = "pink", "M" = "lightblue", "X" = "gray"),
                    labels = c("F" = "Female", "M" = "Male", "X" = "Unknown")) +
  scale_x_discrete(labels = c(
    "A" = "Asian",
    "B" = "Black",
    "H" = "Hispanic",
    "P" = "Pacific Islander",
    "W" = "White",
    "X" = "Unknown"
  ))

crimesbyrace <- LAdatanomissing |>
  group_by(`Vict Sex`, `Vict Descent`) %>%
  summarise(
    Entries = n(),
    .group = 'drop'
  ) 
crimesbyrace <- crimesbyrace[order(crimesbyrace$Entries, decreasing = TRUE),]
crimesbyrace

#LA age distribution

custom_title1 <- c("Number of Crimes Reported by Age in Los Angeles from 2010-2019") 

wrapped_title1 <- sapply(custom_title1, function(x) str_wrap(x, width = 50))

LAage <- LAdatanomissing |>
  mutate(AgeGroup = cut(`Vict Age`, breaks = seq(0, 120, by = 10), right = FALSE, 
                        labels = paste(seq(0, 110, by = 10), seq(9, 119, by = 10), sep = "-")))

plotLAage <- LAage |>
  ggplot(aes(x = `AgeGroup`, fill = `AgeGroup`)) +
  geom_bar(position = "dodge") +
  labs(title = wrapped_title1) +
  labs(x = "Age (Years)", y = "Number of Crimes Reported") +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plotLAage

#Hispanic Female Age Distribution

custom_title2 <- c("Number of Crimes Reported by Hispanic Females by Age in Los Angeles from 2010-2019") 

wrapped_title2 <- sapply(custom_title2, function(x) str_wrap(x, width = 50))

HispanicMales <- LAdatanomissing[ `Vict Sex` == "M" & `Vict Descent` == "H"]
HispanicFemales <- LAdatanomissing[ `Vict Sex` == "F" & `Vict Descent` == "H"]
HispanicFemales[, AgeGroup := cut(`Vict Age`, breaks = seq(0, 120, by = 10), 
                                  right = FALSE, 
                                  labels = paste(seq(0, 110, by = 10), seq(9, 119, by = 10), sep = "-"))]

plotHFage <- HispanicFemales |>
  ggplot(aes(x = `AgeGroup`, fill = `AgeGroup`)) +
  geom_bar(position = "dodge") +
  labs(title = wrapped_title2) +
  labs(x = "Age (Years)", y = "Number of Crimes Reported") +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plotHFage

#Top 5 crimes in Los Angeles
LAcrimes <- LAdatanomissing |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
LAcrimes

Females <- LAdatanomissing[`Vict Sex` == "F"]
Males <- LAdatanomissing[`Vict Sex` == "M"]

#Top 5 crimes done to males
Malecrimes <- Males |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
Malecrimes

#Top 5 crimes done to females
Femalecrimes <- Females |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
Femalecrimes

#Top 5 Crimes reported by hispanic men 
HispanicMalecrimes <- HispanicMales |>
  filter(!is.na(`Crm Cd Desc`) &
           !is.na(`Vict Sex`) &
           !is.na(`Vict Descent`)) |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
HispanicMalecrimes

#Top 5 Crimes reported by hispanic women 
HispanicFemalecrimes <- HispanicFemales |>
  filter(!is.na(`Crm Cd Desc`) &
           !is.na(`Vict Sex`) &
           !is.na(`Vict Descent`)) |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
HispanicFemalecrimes

#Top 5 Crimes reported by hispanic women between 20 and 29
HispanicFemalecrimesAge <- HispanicFemales |>
  filter(!is.na(`Crm Cd Desc`) &
           !is.na(`Vict Sex`) &
           !is.na(`Vict Descent`) &
           AgeGroup == "20-29") |>
  group_by(`Crm Cd Desc`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries)) |>
  top_n(5, Entries)
HispanicFemalecrimesAge

custom_label <- c(
  "BATTERY - SIMPLE ASSAULT" = "Battery - Simple Assault",
  "BURGLARY FROM VEHICLE" = "Burglary from Vehicle",
  "INTIMATE PARTNER - SIMPLE ASSAULT" = "Intimate Partner - Simple Assault",
  "THEFT PLAIN - PETTY ($950 & UNDER)" = "Petty Theft (< $950",
  "VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)" = "Vandalism (> $400)"
) 

wrapped_labels <- sapply(custom_label, function(x) str_wrap(x, width = 15))

custom_title3 <- c("Top 5 Crimes Experienced by Hispanic Females Aged 20-29 in Los Angeles from 2010-2019")
wrapped_title3 <- sapply(custom_title3, function(x) str_wrap(x, width = 50))

HispanicFemalecrimesAge <- HispanicFemalecrimesAge %>%
  mutate(`Crm Cd Desc` = fct_reorder(`Crm Cd Desc`, Entries, .desc = TRUE))

ggplot(data = HispanicFemalecrimesAge, aes(x = `Crm Cd Desc`, y = Entries, fill = `Crm Cd Desc`)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues", labels = custom_label) +
  labs(title = wrapped_title3) +
  labs(x = "Type of Crime", y = "Number of Crimes Reported") +
  scale_x_discrete(labels = wrapped_labels) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(leaflet)
library(viridis)
library(tidycensus)
library(tidyverse)
library(terra)
library(tmap)
library(mapview)
library(rosm)
library(crsuggest)
library(tigris)
library(sf)
library(RColorBrewer)
library(scales)
library(gridExtra)

HispanicFemalemap <- LAdatanomissing |>
  filter(`Vict Sex` == "F" &
           `Vict Descent` == "H" &
           `Vict Age` >= 20 & `Vict Age` <= 29) |>
  group_by(`LAT`, `LON`) |>
  summarize(
    Reports = n(),
    .groups = 'drop') |>
  top_n(100, Reports) |>
  arrange(desc(Reports))

HispanicFemalearea <- LAdatanomissing |>
  filter(`Vict Sex` == "F" &
           `Vict Descent` == "H" &
           `Vict Age` >= 20 & `Vict Age` <= 29 &
           !is.na(`Crm Cd Desc`) &
           !is.na(`Vict Sex`) &
           !is.na(`Vict Descent`) &
           !is.na(`AREA NAME`)) |>
  group_by(`AREA NAME`) |>
  summarize(
    Entries = n(),
    .groups = 'drop') |>
  arrange(desc(Entries))

HispanicFemalearea |>
  ggplot(aes(x = reorder(`AREA NAME`, -Entries), y = Entries, fill = `AREA NAME`)) + 
  geom_bar(stat = "identity") +
  labs(title = "Number of Crimes Reported in Each Area by Hispanic Women Aged 20-29") +
  labs(x = "Precinct Area", y = "Number of Crimes Reported") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

LA_race <- get_decennial(
  geography = "tract",
  state = "CA",
  county = "Los Angeles",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))  %>%
  filter(!st_is_empty(geometry))

LA_hispanic <- filter(LA_race, 
                      variable == "Hispanic")

lapd_divisions <- st_read("C:/Users/ellya/OneDrive/Desktop/Final Project/Final Project/LAPD_Division")

HispanicFemalemap_sf <- st_as_sf(HispanicFemalemap, coords = c("LON", "LAT"), crs = 4326)


LA_hispanic <- st_transform(LA_hispanic, crs = 4326)
lapd_divisions <- st_transform(lapd_divisions, crs = 4326)
HispanicFemalemap <- st_as_sf(HispanicFemalemap_sf, crs = 4326) 


LA_hispanic$Percentage <- label_number(accuracy = 0.1)(LA_hispanic$percent)

combined_data <- st_join(LA_hispanic, lapd_divisions, join = st_intersects)
LAandCensusdata <- st_join(combined_data, HispanicFemalemap, join = st_intersects)


custom_breaks <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

tm_shape(LAandCensusdata) + 
  tm_polygons(col = "percent",
              style = "fixed",
              breaks = custom_breaks,
              palette = "Purples",
              border.alpha = 0.5, 
              lwd = 1,
              popup.vars = c("Percentage", "APREC"),
              title = "Percentage Hispanic (2020)") +
  tm_shape(lapd_divisions) + 
  tm_borders(lwd = 2, col = "black", alpha = 1) +
  tm_shape(HispanicFemalemap) +
  tm_bubbles(col = "Reports",
             palette = "Reds",
             size = 0.01,
             border.alpha = 0.01) +
  tm_view(set.view = c(-118.25, 34.05, 9))

tmap_mode("view")

LAdatanomissing |>
  ggplot() +
  geom_bar(mapping = aes(x = `Vict Descent`, fill = `Vict Sex`), position = "dodge") +
  labs(title = "Number of Crimes Categorized by Race and Gender") +
  labs(x = "Race", y = "Number of Crimes Experienced", fill = "Gender") +
  scale_fill_manual(values = c("F" = "pink", "M" = "lightblue", "X" = "gray"),
                    labels = c("F" = "Female", "M" = "Male", "X" = "Unknown")) +
  scale_x_discrete(labels = c(
    "A" = "Asian",
    "B" = "Black",
    "H" = "Hispanic",
    "P" = "Pacific Islander",
    "W" = "White",
    "X" = "Unknown")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(plotLAage, plotHFage, ncol = 2)

ggplot(data = HispanicFemalecrimesAge, aes(x = `Crm Cd Desc`, y = Entries, fill = `Crm Cd Desc`)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues", labels = custom_label) +
  labs(title = wrapped_title3) +
  labs(x = "Type of Crime", y = "Number of Crimes Reported") +
  scale_x_discrete(labels = wrapped_labels) +
  theme(legend.position = "none")

custom_titlearea <- c("Number of Crimes Reported in Each Area by Hispanic Women Aged 20-29")
wrapped_titlearea <- sapply(custom_titlearea, function(x) str_wrap(x, width = 25))

HispanicFemalearea |>
  ggplot(aes(x = reorder(`AREA NAME`, -Entries), y = Entries, fill = `AREA NAME`)) + 
  geom_bar(stat = "identity") +
  labs(title = wrapped_titlearea, fill = "Precinct Area") +
  labs(x = "Precinct Area", y = "Number of Crimes Reported") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
