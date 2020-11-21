library(httr)
library(jsonlite)

resp <- GET("https://cat-fact.herokuapp.com/facts/random?animal_type=cat&amount=5")
catText<-content(resp, "text")
View(catText)
catJson<-fromJSON(catText, flatten=TRUE)
catDF<-as.data.frame(catText)
print(catDF)

