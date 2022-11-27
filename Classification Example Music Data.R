# Classification Example Using Music Data

setwd("~/Downloads/5415 Advanced Data Analytics")
songs = read.csv("songs-1.csv", stringsAsFactors = T)
-------------------------------------------------------------------------------------------------------------------
# 1. How many observations (songs) are from the year 2010?
table(songs$year)
# OR 
sum(songs$year == 2010)

# ANSWER = 373 songs in 2010
-------------------------------------------------------------------------------------------------------------------
  # 2. How many songs does the dataset include for which the artist name is "Michael Jackson"?
  sum(songs$artistname == "Michael Jackson")

# ANSWER = 18 songs
-------------------------------------------------------------------------------------------------------------------
  # 3. Which songs by Michael Jackson made it to the Top 10?
  
  songs[songs$artistname == "Michael Jackson" &  songs$Top10 == 1, c("songtitle", "Top10") ]

# ANSWER = 5 songs in the top 10 by michael jackson 
-------------------------------------------------------------------------------------------------------------------  
  # 4. The variable corresponding to the estimated time signature (timesignature) is discrete, meaning that it only takes integer values (0, 1, 2, 3, . . . ). 
  # What values of this variable occurred in our dataset?
  
  sort(unique(songs$timesignature))

# ANSWER = 0 1 3 4 5 7
------------------------------------------------------------------------------------------------------------------ 
  # 5. Which timesignature value is the most frequent among songs in our dataset?
  
  table(songs$timesignature)


# ANSWER = 4:     0    1    3    4    5    7 
#                10  143  503 6787  112   19 
------------------------------------------------------------------------------------------------------------------ 
  # 6. Out of all of the songs in our dataset, which song has the highest tempo?
  
  songs$songtitle[which.max(songs$tempo)]

# ANSWER = Wanna Be Startin' Somethin'
------------------------------------------------------------------------------------------------------------------ 
  # 7. We wish to predict whether or not a song will make it to the Top 10. 
  # To do this, first use the subset function to split the data into a training set "SongsTrain" 
  # consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", consisting of the 2010 song releases. How many observations (songs) are in the training set? 
  
  SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
# number of rows in songstrain where year is less than or equal to 2009
nrow(SongsTrain)

# ANSWER =  7201 songs
------------------------------------------------------------------------------------------------------------------ 
  # 8. In this problem, our outcome variable is "Top10" - we are trying to predict whether or not a song will 
  # make it to the Top 10 of the Billboard Hot 100 Chart. Since the outcome variable is binary, we will build a 
  # logistic regression model. We'll start by using all song attributes as our independent variables, which we'll call Model 1.
  # We will only use the variables in our dataset that describe the numerical attributes of the song in our logistic regression model. 
  # So we won't use the variables "year", "songtitle", "artistname", "songID" or "artistID". 
  # To exclude these variables in our dataset from being used as independent variables, use the following R commands:
  nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
# Now, use the glm function to build a logistic regression model to predict Top10 using all of the other variables as the
# independent variables. You should use SongsTrain to build the model.
# Looking at the summary of your model, what is the value of the Akaike Information Criterion (AIC)?

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
glm.fit = glm(Top10 ~ ., data = SongsTrain, family = binomial)
summary(glm.fit)

# ANSWER = AIC: 4827.2
------------------------------------------------------------------------------------------------------------------ 
  # 9. Let's now revisit the variables related to the confidence we have 
  # about time signature, key and tempo(timesignature_confidence, key_confidence, and tempo_confidence). 
  # Our model seems to indicate that these confidence variables are significant 
  # (rather than the variables timesignature, key and tempo themselves). What does the model suggest?
  
  # what is the sign of the coefficents?
  
  timesignature_confidence  7.450e-01  1.953e-01   3.815 0.000136
key_confidence            3.087e-01  1.412e-01   2.187 0.028760
# they are all positive coefficents

# ANSWER = The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10
------------------------------------------------------------------------------------------------------------------ 
  # 10. In general, if the confidence is low for the time signature, tempo, and key, then the song is more likely to be complex. What does Model 1 suggest in terms of complexity?
  
  # ANSWER = Mainstream listeners tend to prefer less complex songs
  ------------------------------------------------------------------------------------------------------------------ 
  # 11. Songs with heavier instrumentation tend to be louder (have higher values in the variable "loudness") and more energetic (have higher values in the variable "energy"). By inspecting
  # the coefficient of the variable "loudness", what does Model 1 suggest?
  loudness                  2.999e-01  2.917e-02  10.282  < 2e-16
# positive coefficents

# ANSWER = Mainstream listeners prefer songs with heavy instrumentation
------------------------------------------------------------------------------------------------------------------ 
  # 12. By inspecting the coefficient of the variable "energy", do we draw the same conclusions as above?
  
  energy                   -1.502e+00  3.099e-01  -4.847 1.25e-06 

# ANSWER = FALSE
------------------------------------------------------------------------------------------------------------------ 
  # 13. What is the correlation between the variables "loudness" and "energy" in the training set?
  
  cor(SongsTrain$loudness, SongsTrain$energy)

# ANSWER =  0.7399067
------------------------------------------------------------------------------------------------------------------ 
  # 14. Given that these two variables are highly correlated, Model 1 suffers from multicollinearity.
  # To avoid this issue, we will omit one of these two variables and rerun the logistic regression. In the rest of this problem, we'll build two variations of our original model: 
  # Model 2, in which we keep "energy" and omit "loudness", and Model 3, in which we keep "loudness" and omit "energy".
  # Create Model 2, which is Model 1 without the independent variable "loudness". This can be done with the following command:
  SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)

# We just subtracted the variable loudness. We couldn't do this with the variables "songtitle" and "artistname", 
# because they are not numeric variables, and we might get different values in the test set that the training set has never seen. But this approach will work when you want to remove variables that 
# you could feasibly use in your model.
# Look at the summary of SongsLog2, and inspect the coefficient of the variable "energy". What do you observe?

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

# ANSWER = Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1.
------------------------------------------------------------------------------------------------------------------ 
  # 15. Now, create Model 3, which should be exactly like Model 1, but without the variable "energy". 
  # Look at the summary of Model 3 and inspect the coefficient of the variable "loudness". Remembering that 
  # higher loudness and energy both occur in songs with heavier instrumentation, do we make the same observation about the popularity of
  # heavy instrumentation as we did with Model 2?
  
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Answer = loudness                  2.306e-01  2.528e-02   9.120  < 2e-16 
# this is still positive and significant
# ANSWER =   TRUE
------------------------------------------------------------------------------------------------------------------ 
  # 16. Make predictions on the test set using Model 3. What is the accuracy of Model 3 on the test set, 
  # using a threshold of 0.45?
  
top10.pred = as.numeric(predict(SongsLog3, newdata = SongsTest,
                                  type = "response") > .45)

table(SongsTest$Top10, top10.pred)

# output
# top10.pred
# 0   1
# 0 309   5
# 1  40  19

(309 + 19) / nrow(SongsTest)

# ANSWER =  accuracy of 0.8793566
------------------------------------------------------------------------------------------------------------------ 
  # 17. Let's check if there's any incremental benefit in using Model 3 instead of a baseline model. 
  # Given the difficulty of guessing which song is going to be a hit, an easier model would be to pick the most 
  # frequent outcome (a song is not a Top 10 hit) for all songs. What would be the fraction of accuracy using this 
  # baseline model for the test set?
  
  sum(SongsTest$Top10 == 0) / nrow(SongsTest)

# ANSWER =  0.8418231
----------------------------------------------------------------------------------------------------------------- 
  # 18. It seems that Model 3 gives us a small improvement over the baseline model. Still, does it create an edge?
  # Let's view the two models from an investment perspective. A production company is interested in investing in songs 
  # that are highly likely to make it to the Top 10. The company's objective is to minimize its risk of financial losses 
  # attributed to investing in songs that end up unpopular.
  # A competitive edge can therefore be achieved if we can provide the production company a list of songs that are highly 
  # likely to end up in the Top 10. We note that the baseline model does not prove useful, as it simply does not label any song 
  # as a hit. Let us see what our model has to offer.
  # How many songs does Model 3 correctly predict as Top 10 hits in 2010, using a threshold of 0.45?
  
 top10.pred = as.numeric(predict(SongsLog3, newdata = SongsTest,
                                  type = "response") > .45)

table(SongsTest$Top10, top10.pred)

# output
# top10.pred
# 0   1
# 0 309   5
# 1  40  19

# ANSWER =  19 
------------------------------------------------------------------------------------------------------------------ 
  # 19. How many non-hit songs does Model 3 predict will be Top 10 hits, using a threshold of 0.45?
  
top10.pred = as.numeric(predict(SongsLog3, newdata = SongsTest,
                                  type = "response") > .45)

table(SongsTest$Top10, top10.pred)

# ANSWER =  5
------------------------------------------------------------------------------------------------------------------ 
  # 20. What is the sensitivity (percentage of true positives that are correctly identified) of Model 3 on the test set, using a threshold of 0.45?
  
  # True positive rate =  
  19 / (40 + 19)

# ANSWER =  0.3220339 True positive rate
------------------------------------------------------------------------------------------------------------------ 
  # 21. What is the specificity (percentage of true negatives that are correctly identified) of Model 3 on the test set, using a threshold of 0.45?
  
  # True Negative = 
  309 / (309 + 5)

# ANSWER =  0.9840764 True Negative
------------------------------------------------------------------------------------------------------------------ 
  # 22. What conclusions can you make about our model (select all that apply)?
  
  # Model 3 favors specificity over sensitivity. TRUE 
  
  # Model 3 favors sensitivity over specificity. FALSE
  
  # Model 3 captures less than half of Top 10 songs in 2010. Model 3 therefore does not provide a useful list of candidate songs to investors, 
  # and hence offers no competitive edge. FALSE
  
  # Model 3 provides conservative predictions, and predicts that a song will make it to the Top 10 very rarely. 
  # So while it detects less than half of the Top 10 songs, we can be very confident in the songs that it does predict to be Top 
  # 10 hits. 

# answer = TRUE

