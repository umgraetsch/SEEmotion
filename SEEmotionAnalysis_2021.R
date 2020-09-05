library (tidyverse)
library(readxl)

#set location of working directory

ws = choose.dir(default = "", caption = "Select Working Director")
setwd(ws)   #set the working directory where the files are stored.
file_name = choose.files(default = "", caption = "Select File with EMTK Emotion Results", multi = FALSE)

resultsdf = read_xlsx(file_name, sheet = "VisionAPIQuestions", col_names = TRUE, col_types = NULL, skip = 1, n_max = 1427, guess_max=10, progress = readxl_progress(), .name_repair = "unique") 
print(resultsdf)

analysis = as_tibble(resultsdf)
analysis = select(analysis, love, joy, surprise, Sadness, fear, anger, none, APIClassification)

love_r = analysis %>%filter(love=="love")%>%count(APIClassification, name='love')
joy_r = analysis %>%filter(joy=="Joy")%>%count(APIClassification, name='joy')
surprise_r = analysis %>%filter(surprise=="surprise")%>%count(APIClassification, name='surprise')
sadness_r = analysis %>%filter(Sadness=="Sadness")%>%count(APIClassification, name='sadness')
fear_r = analysis %>%filter(fear=="fear")%>%count(APIClassification, name='fear')
anger_r = analysis %>%filter(anger=="Anger")%>%count(APIClassification, name='anger')
none_r = analysis %>%filter(none=="NoEmotion")%>%count(APIClassification, name='noEmotion')

output = love_r
output = output%>%left_join(joy_r, by = "APIClassification", suffix =c("love","joy")) 
output = output%>%left_join(surprise_r,by = "APIClassification", suffix =c("joy","surprise")) 
output = output%>%left_join(sadness_r, by = "APIClassification", suffix = c("surprise", "sadness"))
output = output%>%left_join(fear_r, by = "APIClassification", suffix = c("sadness", "fear"))
output = output%>%left_join(anger_r, by = "APIClassification", suffix = c("fear", "anger"))
output = output%>%left_join(none_r, by = "APIClassification", suffix = c("anger", "NoEmotion")) 
output = mutate(output, Total = love + joy + surprise + sadness + fear + anger + noEmotion)%>%arrange(APIClassification)
print(output)


write.csv(output,"SEEmotionAnalysis2021.csv")
