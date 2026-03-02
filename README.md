## Introduction

This project addresses the question "does money buy happiness?" from the perspective of the global “happiness score” and its correlation to GDP per capita. 

The dataset is drawn from the World Happiness Report 2023, a Kaggle.com dataset that offers a comprehensive examination of happiness metrics and the factors influencing well-being on a global scale.

There are 9 CSV files in Kaggle.com, I combined them in one dataset. My dataset includes various indicators related to happiness, covering from 2015 up to 2023:

- country: the name of the country
- region:	the geographic region
- happiness_score: a measure reflecting overall happiness, specifically a subjective well-being measure based on the Gallup World Poll (2005-2022). Respondents rate their life on a 0-10 scale, also known as the Cantril life ladder
- gdp_per_capita: a measure of Gross Domestic Product per capita - an economic output per person in constant 2017 international dollars (PPP); sourced from WDI (2023) with projections for missing years
- social_support: a metric measuring social support based on the percentage of people who report having someone to rely on in times of trouble
- healthy_life_expectancy: expected years of healthy life at birth, based on WHO data (2000-2019) with interpolations for missing years
- freedom_to_make_life_choices: a measure based on national satisfaction with personal freedom to choose life paths
- generosity: a metric reflecting generosity based on residual value from regressing charitable donations on GDP per capita, reflecting altruistic giving.
- perceptions_of_corruption: a measure of perception of corruption within a country, based on average national response to questions on perceived corruption in government and business; sourced from Gallup survey data

The goal of this project is to test whether countries with higher GDP per capita tend to have higher happiness scores while also examining how the relationship between GDP per capita and happiness varies by region and continent.

```{r, warning = FALSE, message = FALSE}
# Loading necessary R packages:
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
```

```{r, echo = FALSE, warning = FALSE, message = FALSE}
data <- read_csv("WHR_2023.csv")
glimpse(data)
```

## Correlation Matrix

```{r, warning = FALSE, message = FALSE}
# Selecting variables for the correlation matrix:
selected_features <- data |>
  select(happiness_score, gdp_per_capita, social_support, 
         healthy_life_expectancy, freedom_to_make_life_choices, 
         generosity, perceptions_of_corruption)

correlation_matrix <- cor(selected_features)

# Selecting colors to represent correlation near 1, 0, -1:
corr_colors <- colorRampPalette(c("orange", "white", "purple"))(200)

# Creating the correlation matrix:
corrplot(correlation_matrix, method = "circle", type = "upper", 
         col = corr_colors, tl.col = "black", tl.cex = 0.8, cl.cex = 0.8, number.cex = 0.7)
```

Based on the correlation matrix above, we can conclude that the happiness score is positively correlated with GDP per capita, social support, and the freedom of choice. 

Let's explore the relationship between happiness score and GDP per capita in different regions.



## Scatterplot (Colored by Regions)

```{r, warning = FALSE, message = FALSE}
# Creating a scatterplot: 
ggplot(data, 
       aes(x = gdp_per_capita, y = happiness_score, color = region)) +
  geom_point(alpha = 1) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  labs(title = "Happiness Score vs GDP per Capita (Colored by Regions)",
       x = "GDP per Capita",
       y = "Happiness Score") +
  theme_classic()
```


This scatterplot proves a positive correlation between Happiness Score and GDP per Capita, indicating that wealthier countries generally report higher happiness levels. 

- Western Europe, North America, and ANZ cluster at the high GDP and high happiness score end, while Sub-Saharan Africa and South Asia are concentrated in the low GDP and low happiness score range. 

- Latin America, the Middle East, and some Asian countries exhibit greater dispersion due to factors beyond GDP per capita that influence happiness scores in these regions. 

- There are outliers in South Asia, Southeast Asia, Middle East & North Africa that deviate significantly from the regression line. These outliers indicate implications of other social, cultural, and political factors on happiness score.

Now, let's explore the relationship between happiness score and GDP per capita in different continents.



## Scatterplot (Colored by Continents)

```{r, warning = FALSE, message = FALSE}
# Updating the dataframe by introducing a new column - continent:
data <- data |>
  mutate(continent = case_when(
    region %in% c("North America and ANZ") ~ "Americas",
    region %in% c("Latin America and Caribbean") ~ "Americas",
    
    region %in% c("Western Europe", "Central and Eastern Europe") ~ "Europe",
    region %in% c("Commonwealth of Independent States") ~ "Europe",
    
    region %in% c("East Asia", "South Asia", "Southeast Asia") ~ "Asia",
    
    region %in% c("Middle East and North Africa") ~ "Africa",
    region %in% c("Sub-Saharan Africa", "Africa") ~ "Africa"
  ))

# Creating a scatterplot colored by continent: 
ggplot(data, aes(x = gdp_per_capita, y = happiness_score, color = continent)) +
  geom_point(alpha = 1, size = 1.5) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  labs(title = "Happiness Score vs GDP per Capita (Colored by Continents)",
       x = "GDP per Capita",
       y = "Happiness Score") +
  theme_classic()
```


This relationship in this scatterplot is consistent with the first scatterplot and also shows a positive correlation between Happiness Score and GDP per Capita.

Higher GDP per capita generally aligns with higher happiness, as indicated by the regression line. Europe and the Americas cluster in the upper range, as these continents have the highest concentration of developed countries. Africa dominates the lower end, with lower GDP per capita and lower happiness levels. Many African countries face economic challenges and limited infrastructure, which may contribute to lower well-being. However, there are outliers in the Middle East, such as Saudi Arabia, a wealthy nation with vast oil reserves. Asia is more spread around the regression line, reflecting a mix of high-income nations like Japan and lower-income countries in South Asia.



## Boxplot

```{r, warning = FALSE, message = FALSE}
# Creating a boxplot: 
ggplot(data, aes(x = region, y = happiness_score, fill = region)) +
  geom_boxplot() +
  labs(title = "Distribution of Happiness Scores by Region",
       x = "Region",
       y = "Happiness Score") +
    theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```


The boxplot shows the distribution of happiness scores across different global regions. 

Western Europe, North America & ANZ have the highest median happiness scores and relatively small spreads, indicating that most countries in these regions report consistently high well-being. For example, Finland, Denmark, and Switzerland often rank among the happiest countries due to strong economies, high social trust, and well-developed welfare systems.

In contrast, Sub-Saharan Africa and South Asia have the lowest median happiness scores, with larger variability and numerous outliers. Countries like South Sudan and Afghanistan consistently score low due to poverty and armed conflicts.

The Middle East and North Africa display high variability, reflecting stark contrasts within the region. With strong economies and high living standards, Saudi Arabia, UAE, Qatar, and Israel report significantly higher happiness scores than countries facing economic crises and political conflict, such as Syria and Yemen. However, Israel reports high happiness scores despite having an armed conflict on its territory. 

Latin America & Caribbean and Central & Eastern Europe have comparable median happiness scores but exhibit different spreads. Latin America often ranks high in happiness, largely due to strong social connections, despite facing economic instability and high crime rates. For example, Costa Rica reports relatively high happiness levels, driven by community bonds and social cohesion, even though their economies are not among the world’s strongest.

In contrast, Central & Eastern Europe experiences moderate happiness levels, but with greater variability, potentially influenced by lower GDP indicators due to the Soviet past and ongoing geopolitical conflicts. Countries like Czech Republic and Lithuania have seen economic growth and rising happiness levels, while Ukraine and Bulgaria face economic struggles and instability. The Soviet past and regional tensions influence happiness levels across Eastern Europe.



## Data Limitation

It is important to keep in mind that some variables in this dataset are based on surveys. In less developed countries, limited access to surveys may lead to underrepresentation. This data limitation can introduce bias into the overall picture of happiness scores in Africa and Asia. Limited survey access in less developed countries means that responses may not fully represent the entire population, particularly in rural areas or conflict zones. As a result, happiness scores from these regions could be underreported or skewed, making direct comparisons with more surveyed regions less accurate.


## Conclusion 

The analysis of the World Happiness Report dataset strongly suggests that money does indeed buy happiness, but with limitations and regional variations. The positive correlation between GDP per capita and happiness scores indicates that wealthier countries tend to report higher happiness levels, especially in Western Europe and North America & ANZ.

However, regional variations exist, with some lower-income regions like Latin America reporting higher happiness than expected due to strong social connections.

Although economic growth is a key driver of happiness, factors like social trust, personal freedom, political climate, and cultural traits also shape well-being, proving that money matters, but it is not the sole determinant of happiness.


## Reference 

Helliwell, J. F., Layard, R., Sachs, J. D., Aknin, L. B., De Neve, J.-E., & Wang, S. (Eds.). (2023). World Happiness Report 2023 (11th ed.). Sustainable Development Solutions Network.
Source: https://www.kaggle.com/code/sinarohani/world-happiness-report-up-to-2023 .
