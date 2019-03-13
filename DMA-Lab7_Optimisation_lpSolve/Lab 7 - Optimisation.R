## G54DMA Lab 7 Optimisation #########
## Jin ZHAO, 12 Mar 2019 #############


## Linear Programing Problems by installing packages 'lpSolve'
install.packages("lpSolve")
install.packages("lpSolveAPI")
library('lpSolve')
library("lpSolveAPI")


########################################################################################################################################################
############################# A.Optimising Farming ############################################################################

# Suppose a farmer has 75 acres on which to plant two crops: wheat and barley. To
## produce these crops, it costs the farmer (for seed, fertilizer, etc.) £120 per acre for the
## wheat and £210 per acre for the barley. The farmer has £15000 available for expenses.
## But after the harvest, the farmer must store the crops while awaiting favourable market
## conditions.  The farmer has storage space for 4000 bushels. Each acre yields an average of 110
## bushels of wheat or 30 bushels of barley. If the net profit per bushel of wheat
## (after allexpenses have been subtracted) is £1.30 and for barley is £2.00, how should the farmer
## plant the 75 acres to maximize profit?”

#### Implement the optimization problem #####################################

#Step1. Create variables / datasets
max_space = 75
max_budget0 = 15000
max_bushels = 4000
crops = data.frame(type = c('wheat', 'barley'), cost=c(120,210),
                   profit = c(1.3,2), bushels = c(110,30))

#Step2. Create an empty model with 0 constraints and 2 decision variables (one for wheat, one for barley)
lpmodel = make.lp(0,2) #

#Step3a.Begin with boundaries: x >= 0, y >= 0
set.bounds(lpmodel, lower=c(0,0))
#Step3b. Add each of the three constraints manually. 
add.constraint(lpmodel, crops$cost, "<=", max_budget0)
add.constraint(lpmodel, crops$bushels, "<=", max_bushels)
add.constraint(lpmodel, c(1,1), "<=", max_space)

#Step4. Set objective function
#set.objfn(lpmodel, c(1.3*110, 2*30)) #hardcoded
set.objfn(lpmodel, crops$bushels*crops$profit) 
#set.objfn(lpmodel, c(1.3, 2)) #INCORRECT: Can you explain why?
## My answer: as the unit of x and y is for acre; the profit is for per bushel. 
## we need to multiple the number of bushel of each acre to make the unit consistence.


#Step5. Set objective function in the correct direction (i.e.minimising or maximising)
lp.control(lpmodel,sense='max')



#Step6. If you want to write your model, to check it later, you can do so with:
write.lp(lpmodel,'farmer_model.lp', type='lp') 

#Step7. Solve the model. If a 0 is returned, an optimal solution is found
solve(lpmodel)

#Step8. Return proposed solution
get.objective(lpmodel) # maximum profit

#Step9. Return proposed configuration
get.variables(lpmodel)   # 21.875 53.125
  

# 1. How many bushels of wheat and barley should the farmer store?
wheat = 21.875*110
barley = 53.125*30
wheat 
barley
#answer: 2406.25 bushels for wheat, 1593.75 bushels for barley


# 2. What is the expected maximum profit in that case?
# answer got from step8,which is 6315.625

# 3. Imagine that in the original scenario, the farmer’s distant aunt has passed away, leaving him £10,000 in her inheritance. 
## How many bushels of wheat and barley should he store in this case? How much profit will he make? What does this result tell you?

max_space = 75
max_budget = 25000
max_bushels = 4000
cropsA2 = data.frame(type = c('wheat', 'barley'), cost=c(120,210),profit = c(1.3,2), bushels = c(110,30))
mdlA3 = make.lp(0,2) 
set.bounds(mdlA3, lower=c(0,0))
add.constraint(mdlA3, cropsA2$cost, "<=", max_budget)
add.constraint(mdlA3, cropsA2$bushels, "<=", max_bushels)
add.constraint(mdlA3, c(1,1), "<=", max_space)
mdlA3
set.objfn(mdlA3, cropsA2$bushels*cropsA2$profit) 
solve(mdlA3)
lp.control(mdlA3,sense='max')
get.objective(mdlA3)  # max profit will be the same,  6315.625
get.variables(mdlA3) # the acre of wheat and barley will be 21.875 and 53.125 seperately


# answer :so he will store 2406.25 bushels for wheat, 1593.75 bushels for barley as same as before, 
## and make the same amount of profit.
## which means: 1) the budget of this farmer is not the current constraints for him, 
## 2) other conditions (the space for planting and the space for storing ) might be the possible limitation that he encountered.




# 4. Imagine that in the original scenario, the farmer is able to access 20 extra acres for his harvest. 
# How many bushels of wheat and barley should he store now? How much profit will he make? What does this result tell you?

max_space = 95
max_budget = 15000
max_bushels = 4000
cropsA4 = data.frame(type = c('wheat', 'barley'), cost=c(120,210),profit = c(1.3,2), bushels = c(110,30))
mdlA4 = make.lp(0,2) 
set.bounds(mdlA4, lower=c(0,0))
add.constraint(mdlA4, cropsA4$cost, "<=", max_budget)
add.constraint(mdlA4, cropsA4$bushels, "<=", max_bushels)
add.constraint(mdlA4, c(1,1), "<=", max_space)
mdlA4

set.objfn(mdlA4, cropsA4$bushels*cropsA4$profit) 
solve(mdlA4)
lp.control(mdlA4,sense='max')
get.objective(mdlA4)  # max profit will be 6460
get.variables(mdlA4) # the acre of wheat and barley will be 20 and 60 seperately

wheat = 20*110
barley = 60*30
wheat 
barley

# answer :so he will store 2200 bushels for wheat, 1800 bushels for barley as same as before, 
## and make profit of 6460
## which means: 1) his acres for harvest was one of the factors that limited his profits. 
## With the increasing of the acres below this number, the profit will increase.
## so next step, we can continuely increase the number of harvesting acre, to see / test if the profit will continuously increase.



# 5. If the farmer had to choose between getting extra funds or extra acres for this harvest, 
## what should he choose if he wanted to maximise profit?

#Answer: Based on the analysis of 3 and 4, he should choose to get extra acres.



# 6. Imagine that in the original scenario, the farmer is able to store 4000 extra bushels.
## How many bushels of wheat and barley should he store now? 
## Howmuch profit will he make? What does this result tell you?

max_space = 75
max_budget = 15000
max_bushels = 8000
crops6 = data.frame(type = c('wheat', 'barley'), cost=c(120,210),profit = c(1.3,2), bushels = c(110,30))
mdl6 = make.lp(0,2) 
set.bounds(mdl6, lower=c(0,0))
add.constraint(mdl6, crops6$cost, "<=", max_budget)
add.constraint(mdl6, crops6$bushels, "<=", max_bushels)
add.constraint(mdl6, c(1,1), "<=", max_space)
mdl6

set.objfn(mdl6, crops6$bushels*crops6$profit) 
solve(mdl6)
lp.control(mdl6,sense='max')
get.objective(mdl6)  # max profit will be 10465.62
get.variables(mdl6) # the acre of wheat and barley will be 71.875 /  3.125

wheat = 71.875*110
barley = 3.125*30
wheat # 7906.25 bushels
barley # 93.75 bushels

#answer: shown above. It means: 1) his stroing bushels was one of the factors that limited his profits. 
## With the increasing of the acres below this number, the profit will increase.
## so next step, we can continuely increase  his stroing bushels, to see / test if the profit will continuously increase.

## ??? sometimes I will get 0 in get.objective() and get.variables for serveral times, 
## but if I keep running it, it will get the answer as shown above.
## Is it caused by the process that the computer is calculating ? 
## How to solve this problem or how do we know the duration required for waiting?





########################################################################################################################################################
############################# B. Minimizing Function ############################################################################

# 1. Write an lpmodel in R to represent this
B1 = data.frame(type=c('c1','c2','c3'), col1=c(6,2,4),col2=c(1,1,6),col3=c(4,5,4))
B1

modelB1 = make.lp(0,3)
modelB1
# No need to use set.bounds(lpmodel,lower=c(0,0,0))
# Add constraints 
row1=150
row2=0
row3=40
add.constraint(modelB1, B1$col1,"<=", row1)
add.constraint(modelB1, B1$col2,">=", row2)
add.constraint(modelB1, B1$col3,"=", row3)
modelB1

# 2. Use Lpsolve to minimise this objective function:
B1 = cbind(B1, col4 = c(-3,-4,-3))
set.objfn(modelB1,B1$col4)
lp.control(modelB1,sense='min')
solve(modelB1)
get.objective(modelB1) # -31 
get.variables(modelB1) # 0 8 0
# 3. Optimise the model again if the equation1 from '<=' change to '='.

modelB3 = make.lp(0,3)
modelB3
# Add constraints 
row1=150
row2=0
row3=40
add.constraint(modelB3, B1$col1,"<", row1)
add.constraint(modelB3, B1$col2,">=", row2)
add.constraint(modelB3, B1$col3,"=", row3)

B3 = cbind(B3, col4 = c(-3,-4,-3))
set.objfn(modelB3,B1$col4)
lp.control(modelB3,sense='min')
solve(modelB3)
get.objective(modelB3) # -31 
get.variables(modelB3) # 0 8 0
# 3. Optimise the model again if the equation1 from '<=' change to '='.

modelB3 = make.lp(0,3)
modelB3

# the modelB3 will be the same with modelB1, as it mentioned:
# 'match.arg(type) : 'arg' should be and only be one of “<=”, “=”, “>=”






########################################################################################################################################################
############################# C. Production Planning Decision ############################################################################

# 1. What are the variables of this problem?
# My Answer: 
## the independent variable are the number of Jean and the number of suit being made per week.
## the dependent variable is the total profit;


# Create variables & datasets
each_wk = 60*8*5
n_cut = 25
n_sew = 35
n_pk = 5
max_cutting = n_cut * each_wk
max_sewing = n_sew * each_wk
max_package = n_pk * each_wk

profit_C = data.frame(type=c('jean','suit'), cutting=c(20,60),sewing=c(70,60),package=c(12,4),each_profit=c(80,120))

profit_C

# 2. Write the linear programming model for this problem.

lpmodel = make.lp(0,2)
lpmodel
# Boundaries: x>=0, y>=0
set.bounds(lpmodel,lower=c(0,0))
# Add constraints 
add.constraint(lpmodel,profit$cutting,"<=", max_cutting)
add.constraint(lpmodel,profit$sewing,"<=", max_sewing)
add.constraint(lpmodel,profit$package,"<=", max_package)
lpmodel

# 3. Calculate the optimal solution for this problem. Write a script in R that
## models this problem. Given the number of staff in each department and
## the profit of suits and jeans, calculates the most cost effective production plan
##and the number of suits and jeans produced per week following such plan.

profit$type
profit$each_profit
# set objective function
set.objfn(lpmodel,profit$each_profit)
# set the objective function to min or max
lp.control(lpmodel,sense='max')

write.lp(lpmodel,'Lab7_Cmodel.lp',type='lp')
solve(lpmodel) # got 0, meaning that an optimal solution is found

get.objective(lpmodel) # answer: 139200
get.variables(lpmodel) # answer: 480, 840


# 4. If the company were to reduce the number of staff in the sewing department 
## from 35 to 25 what would be the most cost effective production plan?
##  Give number of suits and jeans that should be produced per week.
n_sew = 25
max_sewing = n_sew * each_wk
lpmodel2 = make.lp(0,2)
add.constraint(lpmodel2,profit$cutting,"<=", max_cutting)
add.constraint(lpmodel2,profit$sewing,"<=", max_sewing)
add.constraint(lpmodel2,profit$package,"<=", max_package)
lpmodel2
set.objfn(lpmodel2,profit$each_profit)
lp.control(lpmodel2,sense='max')
solve(lpmodel2) # got 0, meaning that an optimal solution is found
get.objective(lpmodel2) # answer: 120000
get.variables(lpmodel2) # answer: 0, 1000


# 5. What would be the production plan if the profit on a pair of jeans was to
## increase to over £100 (choose a reasonable value). Assume all the other
## starting conditions are the same as originally given (i.e. 35 people in sewing). 
## Give the number of suits and jeans that should be produced per week. Answers must be integers. 
profit3= data.frame(type=c('jean','suit'), cutting=c(20,60),sewing=c(70,60),package=c(12,4),each_profit=c(100,120))
n_sew = 35
max_sewing = n_sew * each_wk
lpmodel3 = make.lp(0,2)
lpmodel3
add.constraint(lpmodel3,profit$cutting,"<=", max_cutting)
add.constraint(lpmodel3,profit$sewing,"<=", max_sewing)
add.constraint(lpmodel3,profit$package,"<=", max_package)
lpmodel3
set.objfn(lpmodel3,profit3$each_profit)
lp.control(lpmodel3,sense='max')
solve(lpmodel3) # got 0, meaning that an optimal solution is found
get.objective(lpmodel3) # answer: 148800
get.variables(lpmodel3) # answer: 480 840





########################################################################################################################################################
############################# D. Trading Comoany ############################################################################


# 1. What are the variables of this problem?
# my answer: 12 independent variables, including: c1_in_w1, c1_in_w2, c1_in_w3, c2_in_w1,...,c4_in_w2,c4_in_w3.


# Create variables & datasets
max_w1weight=10
max_w1space=5000

max_w2weight=8
max_w2space=4000

max_w3weight=12
max_w3space=8000

max_c1tonne=18
max_c2tonne=10
max_c3tonne=5
max_c4tonne=20

cargos = data.frame(type=c('c1','c2','c3','c4'), tonne=c(18,10,5,20),volume=c(400,300,200,500),each_profit=c(2000,2500,5000,3500))
cargos

# 2. Write the linear programming model for this problem.

lpmodelD = make.lp(0,12) 
# 0 here means currently there is no constraints;
# 12 here means 12 variables;

lpmodelD
# Boundaries: Limited availability per cargo type
# ‘lower’ must contain one element for each column in the model
set.bounds(lpmodelD,lower=c(0,0,0,0,0,0,0,0,0,0,0,0))

# Add constraints for each wagon
add.constraint(lpmodelD,cargos[c(1,0,0,1,0,0,1,0,0,1,0,0)], '<=', max_w1weight)
add.constraint(lpmodelD,cargos[c(0,1,0,0,1,0,0,1,0,0,1,0)], '<=', max_w2weight)
add.constraint(lpmodelD,cargos[c(0,0,1,0,0,1,0,0,1,0,0,1)], '<=', max_w3weight)

# Add constraints for each cargos' weight (tonne)
add.constraint(lpmodelD,cargos[c(1,1,1,0,0,0,0,0,0,0,0,0)], '<=', max_c1tonne)
add.constraint(lpmodelD,cargos[c(0,0,0,1,1,1,0,0,0,0,0,0)], '<=', max_c2tonne)
add.constraint(lpmodelD,cargos[c(0,0,0,0,0,0,1,1,1,0,0,0)], '<=', max_c3tonne)
add.constraint(lpmodelD,cargos[c(0,0,0,0,0,0,0,0,0,1,1,1)], '<=', max_c4tonne)

lpmodelD

# 3.How much of each cargo type should be loaded on which wagon in order to
## maximize profit? Use R to automatically optimise the equation from the
## previous task.




