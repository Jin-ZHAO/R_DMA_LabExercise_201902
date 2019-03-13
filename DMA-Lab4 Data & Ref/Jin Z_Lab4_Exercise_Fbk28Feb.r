###########################################################
#FOR COMMENTS/FEEDBACK FROM MERCEDES: SEARCH FOR #MERCEDES
##########################################################


# UoN_G54DMA_Lab4_Exercise
# Jin ZHAO
# Update 25 Feb 2019
# use'???' mark the confusing part
# '???'Exist: Yes

########################################################################################################
# A. The Iris Dataset ##################################################################################
########################################################################################################

# A.1.min/max/mean/med/Qu1/Qu2/Qu3 of iris
# MERCEDES: As I have covered in the labs, please try to save the dataset into a variable when you load it. That way, you can modify it and work with it much more flexibly
# Done.Modification: add 'ir=iris'
ir=iris
summary(ir) 

library('dplyr')
# MERCEDES: You missed the first part of the question: "Obtain the same information as in the previous section, but grouping the data into the three classes of plants" For that, you could use aggregate. For example:
# Done.!!!aggregate 
aggregate(ir[,1:4], by=list(ir$Species), summary)
# A.2. Obtain the same information as in the previous section, but grouping the data into the three classes of plants.

# A.2.a.which higher mean sepal length?
summarize(group_by(ir,Species), mean(Sepal.Length)) #MERCEDES: You haven't answered the question: which plant?
# Done. the answer is virginica, with mean 6.59  (setosa is 5.01; versicolor is 5.94)


#A.2.b.the sample with the smaller petal width ?
ag = aggregate(ir$Sepal.Width, by=list(ir$Species), min)
head(ag[order(ag$x),],1) 
### !!!


#A.2.c. the median petal length of the virginica samples?
# Done. 
virginica_spl = filter(ir, Species=='virginica')
summarize(virginica_spl, median(Petal.Length)) #MERCEDES: You haven't defined virginica_spl, right? This will not work.

#MERCEDES: You can use base R for this:
# Done. Better basic R
median(ir[ir$Species=="virginica","Petal.Length"])

#A.2.d.the class of the plant with the highest sepal width?
highest_sw = max(ir$Sepal.Width)
target_row=filter(iris, Sepal.Width==highest_sw) #MERCEDES: This gives an error - check your spelling!
the_class = target_row$Species

print(the_class)

#MERCEDES: This can be done in a much simpler way. For example:
# Done. Better basic R
### ??? what is '-' for ?
head(ir[order(-ir$Sepal.Width),"Species"],1)


#A.3.Select a random sample of Iris with a number of instances between 75 and 100.
iris_spl = sample_frac(iris,0.5) # a random sample with 75 instances
#MERCEDES: This is correct but, how do you do it with base R? For example:
#!!!Better basic R is amazing~ 
iris_spl2=ir[sample(nrow(ir), 75),]
#MERCEDES: You can even go one step further and randomize the number beetween 75 and 100.
num=sample(75:100,1) # get a number between the range randomly.
iris_spl2=ir[sample(nrow(ir),num),]

#A.3.a.
cor(iris_spl$Sepal.Length, iris_spl$Sepal.Width)
### According to the running result, I got a number '-0.02607517'. which means, 
### in the iris_spl dataset, the relationship between Sepal.length and Sepal.Width is weak, 
### it is not a statistically significant relationship. 
#MERCEDES: Would you say weak or non-existent? That result is too close to 0 to be considered weak.
# ???what does this comment mean? should I say non-existent ?

#A.3.b.
cor(iris_spl$Sepal.Length, iris_spl$Petal.Length)
### According to the running result, I got a number '0.8346933'. which means, 
### in the iris_spl dataset, the Sepal length is significantly related to Petal Length.
### The longer the sepal length is, the longer the petal length is.
#MERCEDES: good.


#A.3.c.
cor(iris_spl$Petal.Width, iris_spl$Petal.Length)
### According to the running result, I got a number '0.9623753'. which means, 
### in the iris_spl dataset, the Sepal.length is significantly related to each other.
### The longer the Petal length is, the longer the Petal Width is.


#A.4.a. which species of the sample with the highest ratio?
iris=mutate(iris, petal.ratio = Sepal.Length/Petal.Length)
summarise(group_by(iris,Species),max(petal.ratio))
##The answer is setosa. #MERCEDES: That is great, but where is the code that answers that? You need to return "setosa" in your code...

#A.5.a.Create a new column in the dataset small.setosa, which gives a value of true 
#to setosa plants with petal widths of less than 0.3 cm and false to any other samples.
iris$Small.setosa = ifelse(iris$Species=='setosa'& iris$Petal.Width<0.3,'true','false') 
summarize(group_by(iris,Small.setosa), mean(Petal.Width))
summarize(group_by(iris,Small.setosa), sd(Petal.Width))
summarize(group_by(iris,Small.setosa), median(Petal.Width))
#MERCEDES: This can all be comined through summarise_all, too.


########################################################################################################
# B. The Islands Dataset ################################################################################
###################################################################################################

# B.1.How many island locations have been considered?
length(islands)   # islands is a named vecotor of 48

# B.2.What is the average area of the islands? And the median?
mean(islands)
median(islands)

# B.3 What is the size of the biggest island? And its name?
Maxrow=filter(islands_df, Value==max(islands_df$Value))#MERCEDES: You need to check your code very carefully. You haven't defined islands_df. None of this code will work...
MaxName=Maxrow[1,1]
MaxSize=Maxrow[1,2]

# B.4. And the size of the smallest island? And its name?
Minrow=filter(islands_df, Value==min(islands_df$Value))#islands_df remains undefined...
MinName=Minrow[1,1]
MinSize=Minrow[1,2]

# B.5.How can you obtain both measurements (minimum and maximum area) using only one function?
summary(islands) #MERCEDES: This is obtaining all results. How do you get only those two? For example:
range(islands) #MERCEDES: This will return both min and max
  
# B.6.Calculate the dispersion measurements for all the areas.
summary(islands) #MERCEDES: incorrect. Look at the measurements that are returned in summary. None of them are dispersion measurements

# B.7.How many islands are larger than the third quantile area?
filter(islands_df,Value > 183.2) %>% nrow()#MERCEDES: This is also incorrect. First of all, you still haven't defined islands_df. Second of all, you have hardcoded the value of the third quartile. This is not permitted. Instead, use the quanlite function with base R to index your results
length(islands[islands>quantile(islands,0.75)])


########################################################################################################
# C. The Choco Dataset ################################################################################
#######################################################################################################

choco <- read.csv(file='Desktop/DMA-Lab\ Sheet/DMA-Lab4\ Data\ Analysis/chocolate-bars.csv',header=TRUE,sep=',')#MERCEDES: Check very carefully your bars / and \ is different in R

#MERCEDES: I modified this to read the db from my folder
choco <- read.csv(file='chocolate-bars.csv',header=TRUE,sep=',')

# C.1. How many reviews have been recorded?
sum(choco$Ref)

# C.2. How many attributes for each chocolate bar are collected?
ncol(choco)

# C.3. What is the data type of each attribute?
class(choco$X) # integer
class(choco$Company) # factor
class(choco$Name) # factor
class(choco$Ref) # integer
class(choco$Review.Date) # integer
class(choco$Cocoa.Percent) # integer
class(choco$Company.Location) # factor
class(choco$Rating) # numeric
class(choco$Broad.Bean.Origin) # factor
class(choco$Bean.Type) # factor

# C.4. Obtain the mode from all of the nominal attributes in the dataset.
# Answer: the nominal attributes are the 'factor' ones: Company, Name, Company.Location, Broad.Bean.Origin, Bean.Type
mode(choco$Company)  # numeric
mode(choco$Name) # numeric
mode(choco$Company.Location)  # numeric
mode(choco$Broad.Bean.Origin) # numeric
mode(choco$Bean.Type) #  # numeric
### ??? I don't know why they will be 'numeric'. Aren't they some characters? 
#MERCEDES: R does not have a mode function. The mode function that you are calling calculates something differente. See:
??mode
#MERCEDES: You need to create a mode function yourself and then use that.

# C.5. Obtain the mean, median, Q1 and Q3 from all relevant attributes in the dataset.
summary(choco$Ref)
summary(choco$Review.Date)
summary(choco$Cocoa.Percent)
summary(choco$Rating)

# C.6. How many distinct companies have been considered?
n_distinct(choco$Company)
  
# C.7. How many distinct company locations have been collected?
n_distinct(choco$Company.Location)
  
# C.8. Are there more samples from companies based in the UK or in Peru?
n_uk = filter(choco, Company.Location=='U.K.') %>% nrow()
n_peru = filter(choco, Company.Location=='Peru') %>% nrow()
if(n_uk > n_peru){
  print('More samples from U.K than from Peru, which account for')
  print(n_uk)
}else{
  print('More samples from Peru than U.K, which account for') 
  print(n_peru)
  }
#MERCEDES: This can be simplified a lot. Use base R (which is much more suitable for this exercise) and look at the ifelse instruction.
??ifelse()

# ??? How could I print the text and argument at the same line / in the same print?
# because I found I cannot print two elements in the [ print ('text', n_peru)
# MERCEDES: Good question! For that, I recommend the use of paste. For example:
line =paste("This is some text followed by variables: first var", n_peru, "second var", n_uk)
print(line)

# C.9. Are there chocolate bars from Spanish companies? How many?
n_spanish = filter(choco, Company.Location=='Spain') %>% nrow()   # 24
#MERCEDES: While this is correct, you do not need piping for exercises that are this simple. Piping should be used when you are going to have a lot of auxiliary variables. So, for this exercise, something like this would have been much better:
nrow(choco[choco$Company.Location=="Spain",])

# C.10. Are there chocolate bars from Hungarian companies with a Rating over 3?
n_hungary_rate3 = filter(choco,Company.Location=='Hungary',Rating>=3) %>% nrow() # 17
#MERCEDES: Similar to previous exercise. Base R would have been clearner

# C.11. What is the most common bean origin?
table(choco$Broad.Bean.Origin)
###??? I can tell the most common one based on the observation of the table. 
###??? but how can we code to let the table/data tell us directly ?
#MERCEDES: You need to code a function for the mode (the same as the second exercise).

# C.12. What is the bean origin of the chocolate bar with the highest rating? 
# Where is the company based? If there is more than one chocolate bar with maximum rating, report all bean origins.

highest_rating = filter(choco,Rating == max(choco$Rating))   # got 2rows with rating 5.
the_origin=highest_rating$Broad.Bean.Origin
the_company_location=highest_rating$Company.Location
print(the_origin)  # print the name of the origin with rating 5.

#MERCEDES: You can simplify this considerably with just base R. For example:
choco[choco$Rating==max(choco$Rating),"Company.Location"]

# C.13. Obtain the minimum, maximum and mean of the rating.
summary(choco$Rating)

# C.14. How many Belgian bars are rated 4? And how many French bars?
filter(choco, Rating==4,Company.Location=='Belgium') %>% nrow()  # answer is 6.
filter(choco,Rating==4,Company.Location=='France') %>% nrow() # answer is 23.
#MERCEDES: Simplify this using base R


# C.15. Obtain the centrality and dispersion measurements for the Rating of chocolate bars whose companies are in U.S.A.
### ??? What is 'centrality and dispersion measurements' ?
#MERCEDES: Refer back to the slides from Lecture 4. Centrality measures are mean, median, mode. Dispersion measures are: IQR, Variance.

# C.16. On average, which country has the highest-rated bars?
summarize(group_by(choco,Company.Location),mean(Rating)) %>% arrange() # answer is Amsterdam with 3.5. #MERCEDES: You need your code to return this. Your current implementation does not answer the question that is being asked.
#MERCEDES: base R is much quicker here.

  
# C.17. How many reviews have been carried out each year since 2006?
choco_mt2006 = filter(choco, Review.Date>=2006) %>% group_by(,Review.Date) #MERCEDES: This is incorrect.
year_ref = summarise(group_by(choco_mt2006, Review.Date), sum(Ref))
print(year_ref)
  
# C.18.Which year has the most reviews? Which year has the fewest reviews?
names(year_ref) = c('year','ref')
year_ref = arrange(year_ref,desc(ref))
the_most_year = year_ref[1,1]
print(the_most_year)
year_ref = arrange(year_ref,ref)
the_fewest_year = year_ref[1,1]
print(the_fewest_year)

#MERCEDES: Rethink the previous two exerciese. They can be easily solved with base R. First, aggregate the data according to review date, then sort it, and then return the first and last place. For example
ag=aggregate(choco$Ref, by=list(choco$Review.Date), length)
colnames(ag)=c("year","totals")
ag = ag[order(-ag$totals),]
head(ag,1)
tail(ag,1)

########################################################################################################
# D. The Pulitzer Prize Dataset #######################################################################
#######################################################################################################

pulitzer <- read.csv(file='Desktop/DMA-Lab\ Sheet/DMA-Lab4\ Data\ Analysis/pulitzer-circulation-data.csv', header=TRUE, sep=',')

#MERCEDES: Modified this to read my file
pulitzer=read.csv("pulitzer-circulation-data.csv",header=TRUE)

# D.1. How many journals have been considered?
nrow(pulitzer)  # 50
  
# D.2. How many attributes have been collected per publications?
ncol(pulitzer)  # 7
  
# D.3. What is the type of the data collected?
names(pulitzer)=c('Newspaper','DC2004','DC2013','Change.DC04to13','WF1.1990to2003','WF2.2004to2014','WF3.1990to2014')
class(pulitzer$Newspaper) # Newspaper  - "factor"
class(pulitzer$DC2004) # Daily Circulpuation 2014 - "factor"
class(pulitzer$DC2013) # Daily Circulation 2003 -"factor"
class(pulitzer$Change.DC04to13) # Change in Daily Circulation -"factor"
class(pulitzer$WF1.1990to2003) # Winners and Finalists 1990-2003  - "integer"
class(pulitzer$WF2.2004to2014) # Winners and Finalists 2004-2014  - "integer"
class(pulitzer$WF3.1990to2014) # Winners and Finalists 1990-2014  -"integer"

# Convert the data to numeric by deleting '%' and ','
pulitzer$DC2004 = as.numeric(gsub(",","",pulitzer$DC2004))
pulitzer$DC2013 = as.numeric(gsub(",","",pulitzer$DC2013))
pulitzer$Change.DC04to13 = as.numeric(gsub("%","",pulitzer$Change.DC04to13))#MERCEDES: Very good!
### !!! Important to delete 'commas' or others within the numbers, which R cannot distinguish.

# D.4. Calculate centrality and dispersion measurements from all of the appropriate attributes.
### ??? What is 'centrality and dispersion measurements' ?
#MERCEDES: Please refer to my previous anwer re: centrality/dispersion measures.

# D.5. What is the total circulation of all of the journals considered?
sum(pulitzer$DC2004)  # answer: 23121087
sum(pulitzer$DC2013) # answer: 18123142

# D.6. How many publications have experience a positive increase in their circulation?
filter(pulitzer,Change.DC04to13>0) %>% nrow() # answer: 6
#MERCEDES: This is an overcomplication. Use base R. Piping and dplyr are unnecessary here.

# D.7. What is the publication with more winners and finalists overall?
publication_mostwinners = filter(pulitzer, WF3.1990to2014 == max(WF3.1990to2014)) 
publication_mostwinners[1,1]   # answer is New York Times #MERCEDES: use $ to access relevant fields in dataframes.

# D.8. And the publication with more winners and finalists between 1990 and 2003?
publication_mostwinners = filter(pulitzer, WF1.1990to2003 == max(WF1.1990to2003)) 
publication_mostwinners[1,1]   # answer is New York Times
  
# D.9. What is the range in the circulation in 2004? And in 2013?
summary(pulitzer$DC2004)
summary(pulitzer$DC2013)
#MERCEDES: This does not answer the question: what is the range?

# D.10. How many journals experienced a decrease in circulation of over 40%?
filter(pulitzer, Change.DC04to13 <= -40) %>% nrow() # answer is 16
#MERCEDES: Use base R, this is too cumbersome for a simple calculation

#
# D.11. What is the journal with most finalist and winners? The same as 7
publication_mostwinners = filter(pulitzer, WF3.1990to2014 == max(WF3.1990to2014)) 
publication_mostwinners[1,1]   # answer is New York Times

# D.12. How many journals had over 30 candidates or winners between 1990-2003 
# and between 2004-2014. Which journals are these?
filter(pulitzer, WF1.1990to2003 >= 30 ) %>% nrow()  # answer is 4
journal_over30_WF1 = filter(pulitzer, WF1.1990to2003 >= 30)
journal_over30_WF1[,1] # answer: Wall Street Journal, New York Times, Los Angeles Times,Washington Post
  
filter(pulitzer, WF2.2004to2014 >= 30 ) %>% nrow()  # answer is 3
journal_over30_WF2 = filter(pulitzer, WF2.2004to2014 >= 30)
journal_over30_WF2[,1] # answer: New York Times    Los Angeles Times Washington Post  

# D.13. How many journals with a daily circulation of over 1 million in 2004 
# have received more than 30 nominations or prizes since 2004?
filter(pulitzer, DC2004>= 1000000, WF2.2004to2014) %>% nrow() # answer is 3. 
#MERCEDES: This is incorrect, you are not checking for one of the conditions (that they need to have at least 30 nominations.)
