## Conjoint Creator Code
## Stephanie Pedron (pedron.2@osu.edu)

rm(list = ls())
library(tidyverse)


########################   GENERATE EXPERIMENT PROFILES   ##############

## Attributes and Levels
gender <- as.factor(c("Male", "Female"))
education <- as.factor(c("college degree", "high school diploma", "less than high school"))
past_conviction <- as.factor(c("prior conviction", "no prior conviction"))
country_origin <- as.factor(c("Mexico", "USA", "China", "Germany", "Syria", "Guatemala"))
citizenship <- as.factor(c("Yes", "No"))
english <- as.factor(c("Fluent English", "Broken English"))

## Generating all possible profiles
experiment <- expand.grid(gender = gender, education = education, past_conviction = past_conviction, 
                          country_origin = country_origin, citizenship = citizenship,
                          english = english) # 288 profiles

## Restricting profiles
experiment <- experiment %>% 
  filter(country_origin != "USA" | citizenship != "No") %>% 
  filter(country_origin != "USA" | english != "Fluent English") # 252 profiles


## Generating two random profiles for IRR
set.seed(1055385644)
random_profiles <- sample(nrow(experiment), size = 2, replace = T)
random_profiles <- experiment[random_profiles,]

########################   JAVASCRIPT CODE FOR QUALTRICS   ##############
## Javascript Code for Qualtrics
# /* Code Reference: https://m-graham.com/resources/conjoint%20how-to.pdf */
#   
#   /* Randomization Code
# Note: The randomization script was placed within the gender question on Qualtrics */
# 
#   /* Ten choice randomization of immigrant profiles */
# 
# Qualtrics.SurveyEngine.addOnload(function()
# {
#   
#   /* number of choices */
#     var numChoice = 10;
#     
#     /* attributes and levels */
#       
#       var genderArray = ["Male", "Female"];
#       var educationArray = ["Less than high school", "High school diploma", "College degree"];
#       var countryArray = ["Mexico", "USA", "China", "Germany", "Syria", "Guatemala"];
#       var englishprofArray = ["Broken English", "Fluent English"];
#       var convictionArray = ["Prior conviction (not voting related)", "No prior conviction"];
#       var citizenArray = ["Yes", "No"];
#       
#       /* shuffle */
#         function shuffle(array) {
#           for (var i = array.length - 1; i > 0; i--) {
#             var j = Math.floor(Math.random() * (i + 1));
#             var temp = array[i];
#             array[i] = array[j];
#             array[j] = temp;
#           }
#           return array;
#         }
#       
#       /* shuffle vector and choose first entry */
#         function shuffle_one(theArray) {
#           var out = shuffle(theArray);
#           var out = out[0];
#           return out;
#         };
#       
#       /* perform randomization and save result */
#         for (i = 1; i <= numChoice; i++) {
#           var usaIndex = countryArray.indexOf("USA");
#           var selectedCountry = shuffle_one(countryArray);
#           var selectedEnglish = (selectedCountry === "USA") ? "fluent English" : shuffle_one(englishprofArray);
#           var selectedCitizen = (selectedCountry === "USA") ? "Yes" : shuffle_one(citizenArray);
#           
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_gender1", shuffle_one(genderArray));
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_gender2", shuffle_one(genderArray));
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_education1", shuffle_one(educationArray));
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_education2", shuffle_one(educationArray));
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_education3", shuffle_one(educationArray));
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country1", selectedCountry);
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country2", selectedCountry);
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country3", selectedCountry);
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country4", selectedCountry);
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country5", selectedCountry);
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country6", selectedCountry);
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_englishprof1", selectedEnglish);
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_englishprof2", selectedEnglish);
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_conviction1", shuffle_one(convictionArray));
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_conviction2", shuffle_one(convictionArray));
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_citizen1", selectedCitizen);
#           Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_citizen2", selectedCitizen);
#         }
# });

########################   HTML CODE FOR QUALTRICS   ##############
## HTML Code for Qualtrics - Cojoint Block
# Code Reference: https://m-graham.com/resources/conjoint%20how-to.pdf
# 
# Notes: Refer to Excel file for loop and merge grid used for conjoint blocks.
#        Make sure the right Fields align with the variables in the loop & merge grid.
# 
# 
# Conjoint table presentations of immigrant voter profiles
# 
# <center>
# <table class = "wholetable" style="min-width:60%">
# <tr class = "toprow">
# <td></td>
# <td class="attr"><b>Profile 1 </b> </td>
# <td class="attr"><b>Profile 2 </b> </td>
# </tr>
# <tr class="normalrow">
# <td class="name">Gender</td>
# <td class="attr">${lm://Field/1}</td>
# <td class="attr">${lm://Field/2}</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Education</td>
# <td class="attr">${lm://Field/3}</td>
# <td class="attr">${lm://Field/4}</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Country of Origin</td>
# <td class="attr">${lm://Field/6}</td>
# <td class="attr">${lm://Field/7}</td>
# </tr>
# <tr class="normalrow">
# <td class="name">English Proficiency</td>
# <td class="attr">${lm://Field/12}</td>
# <td class="attr">${lm://Field/13}</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Conviction Status</td>
# <td class="attr">${lm://Field/14}</td>
# <td class="attr">${lm://Field/15}</td>
# </tr>
# <tr class="normalrow">
# <td class="name">US Citizen</td>
# <td class="attr">${lm://Field/16}</td>
# <td class="attr">${lm://Field/17}</td>
# </tr>
# </table>
# </center>



## HTML Code for Qualtrics - IRR Profiles
## TABLE 1
# <center>
# <table class = "wholetable" style="min-width:60%">
# <tr class = "toprow">
# <td></td>
# <td class="attr"><b>Profile 1 </b> </td>
# <td class="attr"><b>Profile 2 </b> </td>
# </tr>
# <tr class="normalrow">
# <td class="name">Gender</td>
# <td class="attr">Male</td>
# <td class="attr">Female</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Education</td>
# <td class="attr">Less than high school</td>
# <td class="attr">Less than high school</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Country of Origin</td>
# <td class="attr">Mexico</td>
# <td class="attr">Guatemala</td>
# </tr>
# <tr class="normalrow">
# <td class="name">English Proficiency</td>
# <td class="attr">Fluent English</td>
# <td class="attr">Fluent English</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Conviction Status</td>
# <td class="attr">Prior conviction</td>
# <td class="attr">Prior conviction</td>
# </tr>
# <tr class="normalrow">
# <td class="name">US Citizen</td>
# <td class="attr">Yes</td>
# <td class="attr">No</td>
# </tr>
# </table>
# </center>

# TABLE 2
# <center>
# <table class = "wholetable" style="min-width:60%">
# <tr class = "toprow">
# <td></td>
# <td class="attr"><b>Profile 1 </b> </td>
# <td class="attr"><b>Profile 2 </b> </td>
# </tr>
# <tr class="normalrow">
# <td class="name">Gender</td>
# <td class="attr">Female</td>
# <td class="attr">Male</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Education</td>
# <td class="attr">Less than high school</td>
# <td class="attr">Less than high school</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Country of Origin</td>
# <td class="attr">Guatemala</td>
# <td class="attr">Mexico</td>
# </tr>
# <tr class="normalrow">
# <td class="name">English Proficiency</td>
# <td class="attr">Fluent English</td>
# <td class="attr">Fluent English</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Conviction Status</td>
# <td class="attr">Prior conviction</td>
# <td class="attr">Prior conviction</td>
# </tr>
# <tr class="normalrow">
# <td class="name">US Citizen</td>
# <td class="attr">No</td>
# <td class="attr">Yes</td>
# </tr>
# </table>
# </center>