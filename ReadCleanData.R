rm(list = ls())

require(doParallel)
set.seed(1987)
require(tidyverse)
require(VIM)
require(corrplot)
require(mice)
require(caret)
require(readtext)


##################
## read in data ##
# from Kaggle https://www.kaggle.com/c/house-prices-advanced-regression-techniques
dat.train <- read.csv("D:/Research/R learning/Kaggle/data/train.csv")
dat.test <- read.csv("D:/Research/R learning/Kaggle/data/test.csv")
str(dat.train)
str(dat.test)

# Is this what you mean? I think the best we can do is read as csv and the format wide if we want?
data_description <- read.csv("D:/Research/R learning/Kaggle/data/data_description.txt", stringsAsFactors = FALSE)

# head(read.csv("D:/Research/R learning/Kaggle/data/sample_submission.csv"))

colnames(dat.train)[!colnames(dat.train)%in%colnames(dat.test)]
y.train <- dat.train$SalePrice
dat.train <- dat.train[colnames(dat.train)!="SalePrice"]
table(colnames(dat.train)%in%colnames(dat.test))
##################

################
## Misingness ##

# require(tidyverse)
missing <- dat.train %>%
  select_if(function(x) {any(is.na(x))})

# require(VIM)
aggr(missing, numbers = TRUE, prop = c(TRUE, FALSE))

(to_rem <- dat.train %>%
    select_if(function(x) {sum(is.na(x))>nrow(dat.train)*0.2}) %>%
    summarise_all(funs(sum(is.na(.))))
)


# Instead of being super fancy:
View(data_description[[1]][grep("NA", data_description[[1]])])

# tidyer way of replacing factor. Couldn't get transmute to work though :(

missing <- as.data.frame(
  dat.train %>% 
  select_if(function(x) {any(is.na(x)) & is.factor(x)}) %>% 
  apply(., 2, function(y){
    recode_factor(y, .missing = "None")})
  )

dat.train <- dat.train[!colnames(dat.train) %in% colnames(missing)]

#think its in the same order, base code you can clean :)

dat.train <- bind_cols(dat.train, data.frame(missing))


# these are truly missing?
colnames(dat.train %>%
           select_if(function(x) any(is.na(x))))

# Lets check

apply(dat.train %>%
        select_if(function(x) {any(is.na(x))})
      , 2, min, na.rm = TRUE)

# Lot Frontage not a panhandle?

# No idea what masonry veneer area is but it looks like truly missing because min value is zero

# Garage Year Built is NA when there is no Garage but its not missing data
identical(
dat.train$GarageYrBlt[dat.train$GarageYrBlt == "NA"] == "NA",
dat.train$GarageYrBlt[dat.train$GarageType == "None"] == "NA"
)

# ok so same conclusion as below, your method is better but choice of variable is worse :)

# system("grep -A 2 -B 2 GarageYrBlt data/data_description.txt")
table(is.na(dat.train$GarageYrBlt), dat.train$GarageCars==0)
#so this isnt there as they is no garage

# system("grep LotFrontage data/data_description.txt")
table(lotFront = is.na(dat.train$LotFrontage), lotArea = dat.train$LotArea>0)
# ah, one truley missing! Lets impute this later
# Nope, lot area doesn't need frontage

# system("grep Mas data/data_description.txt")

table(is.na(dat.train$MasVnrArea), dat.train$MasVnrType)

# But why only 8 out of 864 have missing values? surely these should be zero if masonry veneer type is "none"?

View(dat.train$MasVnrArea[dat.train$MasVnrType == "None"])

# Ok very confusing. All zero except for the NAs and like 5 weird outliers...

aggr(dat.train %>%
       select_if(function(x) any(is.na(x))),
     numbers = TRUE, prop = c(TRUE, FALSE))
