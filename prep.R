
library( data.table )
library( ggplot2 )

BFsales <- fread( "/home/caterina/Documents/TDL_PrivateRepos/BlackFridayShinyApp/BlackFriday.csv" )

BFsales[ , User_ID := as.factor( User_ID ) ]

BFsales[ , Product_ID := as.factor( Product_ID ) ]

BFsales[ , Gender := as.factor( Gender ) ]
levels( BFsales$Gender ) <- c( "Female", "Male" )

BFsales[ , Stay_In_Current_City_Years := ordered( Stay_In_Current_City_Years, levels = sort( unique( Stay_In_Current_City_Years ) ) ) ]

BFsales[ , Marital_Status := factor( Marital_Status ) ]
levels( BFsales$Marital_Status ) <- c( "Married", "Single" )

BFsales[ , Product_Category_1 := as.factor( Product_Category_1 ) ]
BFsales[ , Product_Category_2 := as.factor( Product_Category_2 ) ]
BFsales[ , Product_Category_3 := as.factor( Product_Category_3 ) ]

BFsales[ , Age := ifelse( Age == "0-17", "11-17", Age ) ]
BFsales[ , Age := ifelse( Age == "55+", "56-85", Age ) ]
BFsales[ , AgeMin := as.numeric( gsub( "([0-9]+)?.*", "\\1", Age ) ) ]
BFsales[ , AgeMax := as.numeric( gsub( "^.*-([0-9]+)$", "\\1", Age ) ) ]
BFsales[ , Age := ordered( Age, levels = sort( unique( Age ) ) ) ]
runif_vectorized <- Vectorize( runif )
BFsales[ , AgeNum := runif_vectorized( 1, min = AgeMin, max = AgeMax ) ]

head( BFsales )


# By person
random_customers <- sample( unique( BFsales$User_ID ), 4, replace = FALSE )
ggplot( BFsales[ User_ID %in% random_customers, ], aes( x = AgeNum, y = Purchase ) ) + 
  geom_line( ) + 
  facet_grid( .~ User_ID, scales = "free_x" )


# What product category seems to suit what ages??
purchase_by_age_agr <- aggregate( Purchase ~ Age + Gender + Product_Category_1 + Marital_Status, data = BFsales, FUN = mean )
ggplot( purchase_by_age_agr, aes( x = Product_Category_1, y = Purchase, group = Age, color = Age ) ) + 
  geom_line() + facet_grid( Marital_Status ~ Gender )





