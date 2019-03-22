
library( data.table )
library( ggplot2 )
library( viridis )

BFsales <- fread( "BlackFriday.csv" )

BFsales[ , User_ID := as.factor( User_ID ) ]

BFsales[ , Product_ID := as.factor( Product_ID ) ]

BFsales[ , Occupation := as.factor( Occupation ) ]

BFsales[ , Gender := as.factor( Gender ) ]
levels( BFsales$Gender ) <- c( "Female", "Male" )

BFsales[ , Stay_In_Current_City_Years := ordered( Stay_In_Current_City_Years, levels = sort( unique( Stay_In_Current_City_Years ) ) ) ]

BFsales[ , Marital_Status := factor( Marital_Status ) ]
levels( BFsales$Marital_Status ) <- c( "Married", "Single" )

BFsales[ , Product_Category_1 := as.factor( Product_Category_1 ) ]
BFsales[ , Product_Category_2 := as.factor( Product_Category_2 ) ]
BFsales[ , Product_Category_3 := as.factor( Product_Category_3 ) ]

BFsales[ , Age := ifelse( Age == "0-17", "Under 17", Age ) ]
BFsales[ , Age := ifelse( Age == "55+", "Over 55", Age ) ]
BFsales[ , Age := ordered( Age, levels = c( "Under 17", "18-25", "26-35",  "36-45", "46-50", "51-55","Over 55" ) ) ]

head( BFsales )




# What product category seems to suit what ages??
purchase_by_age_agr <- aggregate( Purchase ~ User_ID + Age + City_Category, data = BFsales, FUN = sum )
purchase_by_age_agr <- aggregate( Purchase ~ Age + City_Category, data = purchase_by_age_agr, FUN = mean )

ggplot( purchase_by_age_agr, 
        aes( x = City_Category, y = Purchase, group = Age, color = Age ) ) + 
  geom_point( size = 2.5 ) +
  geom_line( lwd = 1.5 ) +
  scale_color_viridis_d( direction = -1, begin = 0.20, end = 0.85, option = "B" ) +
  labs( x = "City category",
        color = "Age band" ) +
  ggtitle( "Average spend according to customer age and city type",
           subtitle = "- Add note here -" )


# Now to investigate this further with an interactive app. 
# We can tackle questions such as:
## Is this relationship even more abrupt when looking at people residing in these cities for 4+ years, compared to 1 year?
## Does product category affect this relationship?
## Marital status, gender, occupation - any influence?

