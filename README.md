# PPHA 30536 Data and Programming for Public Policy II - R Programming, Autumn 2022

## Project Description:
Research Question: I used data from Current Population Survey to analyze the gender wage pay gap in the US from 2000 to 2020. Research has shown that although female's labor participation rate has being increasing, the gender wage gap is not narrowing down as expected. According to Claudia Goldin, between 1970s to 1990s, with the increasing labor force participation, there was a huge narrowing down of gender wage gap. However, the increase seems to stall starting the 21th century, as the labor force increase started to be steady. The wage gap still exists even after holding marital status, number of children etc. Therefore, I want to take a closer look at the wage gap between women and men within different socioeconomic groups and geographic locations to find out the underlying factors and changing trend.

1. __Dataset__  
There are three major datasets used:[Get Data Here](https://drive.google.com/drive/folders/1y3B0fA5AhIfY8nG5gEL9ng8BOwPORu4J?usp=sharing)
 * `Current Population Survey`, US Bureau of Statistics and NBER. CPS is a monthly survey among more than 25,000 households conducted by US Bureau of Statictics and Cencus. I used the merged outgoing households group subset of CPS compiled by NBER, as the dataset is widely used among research on income inequality. The final merged and cleaned dataset includes 1,910,147 samples from 2000 to 2020 on their labor force status and socioeconomic characteristics. Variables including: 
   * Labor force major variables: Working hours, earnings per week
   * Labor force disaggregation variables: working sector, industry, occupation
   * Socio variables: age, sex, race, highest degree earned, marital status, number of children
   * Geographic variables: residential state, state FIPS code
 * `State Level economic Data`
   * Economy Data: GDP, GDP per capita, unemployment rate, poverty rate: Retrieved with API from St Louis Fed
 * `State Level socio Data`
   * Politics data: state partisanship attitude by presidential election from 2000 to 2020; Retrieved with API from [MIT Election Data + Science Lab](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ)
   * Women Fertility data: fertility rate of women; Downloaded from [CDC Natality Database](https://wonder.cdc.gov/natality.html) and merged all datasets from 2003 to 2020.

 2. __Plotting__
 [Shiny Dashboard here](https://weiluj-uchicago.shinyapps.io/final-project-weiluj/?_ga=2.26338805.996254604.1674329823-442783070.1674329823/)
 * Static plot:
 `Overall`
   * Time Series Plot of Women and Men's Median Wage Trend in the US
   * Time Series Plot of Women's Median Wage as a Percentage of Men
   * Time Series Plot of Women's Median Wage as a Percentage of Men by Education Level, Age Group, Race, in the past 20 years
   * Time Series Plot of Women's Median Wage as a Percentage of Men by Number of children and Marital Status in the past 20 years 
`State Level`
   * Hexbin spatial plot of Women's Overall Median Wage Compared to Men
   * Comparison of several state level socioeconomic characteristics, including *fertility & gdp per capita growth*, *fertility & poverty/unempolyment rate*, *fertility & state partisanship attitude by presidential election*
   * Lowest Income Female Group Plot by Race in each State and Lowest Equal Pay Female Group Plot
 * Interative plot:
   * Choropleth of Women's Median Wage as a Percentage of Men in the US by State in the past 20 years
   * Scatter Plot of State *Fertility Rate & Partisanship* / *Fertility Rate & Partisanship* and Gender Wage Gap
   * Bar plot of Women's Median Wage as a Percentage of Men in the US by industry and occupation with lowest pay gap in the past 20 years
 4. __Text processing__
 * With the twitterR packages, I searched for discussion related to "gender pay" in twitter from 2018 to 2022 and conducted basic sentiment analysis. I didn't analyze data from earlier years as twitter is not as widely used.
 5. __Analysis__
  * I used OLS model to analyze the result, with interaction between *marital* and *ownchild*. I excluded some other interaction groups as I didn't find significant effect by running the model. The model shows that state level control does not has signifincat effect on gender wage gap. Personal characteristics including age, eudcation and marital status have much higher effect.
  * Importantly, after controlling for all variables, *sex_male* is still the most significant variables, which has a coefficient of 181. i.e., male in the same age group, marital status, eduation level , to name a few, with women earn $181 more weekly on median compared to woman in the same group. The R square for the final model is 0.355.
  * Besides *sex*, *highest education degree earned* has the second largest effect on median wage, which is aligned with the exploratory analysis above. Surprisingly, *unemployment rate* has a significant positive effect on median wage.

## Challenges:
 * a. The major challenge is that survey data has different versions and has different variable naming, factor levels and sample size, so it's hard to decide how to categorize variables and clean data. For the CPS survey, every 3-5 years there are changes being made among others. I ended up choosing variables shared across most years 
   * 2003: occupation code occ80 changed to version 2000 occ00
   * 2006: column order change, remove veteran and add vet1:4
   * 2011: occupation code changed to version 2011 occ2011
   * Race does not include Hispanic people, which are being categorized by *Ethnicity*. Besides, after 2004, CPS started to include more detailed information on Race, changing from the general 4 groups category to more diverse options, including *Black-White*, *2 or 3 races*, *4 or 5 races* etc.
   * In addition, same variables could be coded in different class over years, which caused difficulty in merging/joining data. I checked the *data distionary* provided by NBER for each year to make sure the final data sharing the same categorizing rule.
   * Besides, the survey data includes some outliers/abnormal data. Some people have 9 children/have weekly income greater than 2000. I filtered some obvious outlier data at first, and then used *fct* function to categorize them to normal groups. When plotting, I used *winsorize* to format the plot 
 * b. Since the Fertility Rate Data is also retrieved from Survey by CDC, similar issue occurs. There is no available data by any geo disaggregation if downloading the data wit *API*, and they separated the survey site by version so it's hard to scraping the data as well.. Therefore, I downloaded the data (text file only, csv files are not compatiable) manually from each year and merged it. Still, no fertility data befor 2003 can be get at state level from CDC.
 * c. I want to look at the effect of partisanship on gender wage gap, while data is hard to find. There are some social index data for each states, but those are only available after ~2015. Using presidential election results could be an imperfect index to evaluate partisanship, and since election only occurs every four years, it's hard to capture the effect on changes ocurred between elections.

## Results
 * First, although women's wage continue to increase in the past 20 years, the gender wage gap still occurs. In general, females are paied 20 % less compared to men. However, the gap is narrowing down comapared to 20 years ago. In 2000, women were only paid 65% as of their male counterparts. Increase started to occur between 2008 to 2014, while in recent years the increase seemed to slow down.
 * Among different female groups, a general trend is that women who earn the most also share the highest Gender Wage Gap. For example, women between 35-54 have the highest median wage, while they also have the lowest pay ratio compared to men in the same age group. Same applies to *Race*, across all years and nearly all states, either White or Asian Female earns the most, while *White Women* has the lowest wage ratio, given that *White Men* has the highest median income among all groups. Black women earns the least by median wage but are being paid most equally compared to male in the same group; Women in *Management* occupation also share high pay but low waga ratio as to male.
 * It's worthnoting that the seemingly negative relation between earning more and being paid equally does not apply to education. Women with advanced degree earns the most and are also paid more equally. Marriage does not affect women if looking at median wage, while they earn much less on ratio across the years compared to never marreid female.
 * State Level analysis indicates that Blues states are slightly more likely to have more euqal pay compared to Red States. WY and UT have the lowest wage ratio across the past 20 years. IL is among the median compared to other states. Generally, women living in East Coast are being paid more equally, with DC being one of the top.

### Appendix-Code Organization
 * Data
   * Raw data uploaded to Github, and final merged data can be accessed via *Google Drive* 
 * Code
   * data_cleaning.R : clean survey data
   * analysis & model.R : Exploratory Analysis and Model Building
   * app.R : Building Shiny Dashboard
   * text_processing.R : Sentiment Analysis
 * Plots
   * All plots included under `images` folder
 * Write up in `ReadMe.RMD`

