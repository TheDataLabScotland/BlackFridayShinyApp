
# Packages ----------------------------------------------------------------

library( data.table )
library( ggplot2 )



shinyApp(
  
  
  
  # UI ----------------------------------------------------------------------
  
  ui = fluidPage(
    
    titlePanel( "Black Friday sales" ),
    br(),
    sidebarLayout(
      
      sidebarPanel( width = 3,
                    
                    uiOutput( "productCategories" )      # show all product categories
                    
      ),
      
      mainPanel ( width = 9,
                  plotOutput( outputId = "linePlot", width = 640 ),
                  br(),
                  tableOutput( outputId = "dataDescr" ),
                  br(),
                  verbatimTextOutput( outputId = "info" )
                  
      )
      
    )
    
  ),
  
  
  
  
  # Server ------------------------------------------------------------------
  
  
  
  server = function( input, output, session ) {
    

    getData <- reactive({

      BFsales <- fread( "/home/caterina/Documents/BlackFridayShinyApp/BlackFriday.csv" )
      BFsales[ , User_ID := as.factor( User_ID ) ]
      BFsales[ , Product_ID := as.factor( Product_ID ) ]
      BFsales[ , Occupation := as.factor( Occupation ) ]
      BFsales[ , Gender := as.factor( Gender ) ]
      levels( BFsales$Gender ) <- c( "Female", "Male" )
      BFsales[ , Stay_In_Current_City_Years := ordered( Stay_In_Current_City_Years, 
                                                        levels = sort( unique( Stay_In_Current_City_Years ) ) ) ]
      
      BFsales[ , Marital_Status := factor( Marital_Status ) ]
      levels( BFsales$Marital_Status ) <- c( "Married", "Single" )
      BFsales[ , Product_Category_1 := as.factor( Product_Category_1 ) ]
      BFsales[ , Product_Category_2 := as.factor( Product_Category_2 ) ]
      BFsales[ , Product_Category_3 := as.factor( Product_Category_3 ) ]
      
      BFsales[ , Age := ifelse( Age == "0-17", "11-17", Age ) ]
      BFsales[ , Age := ifelse( Age == "55+", "56-85", Age ) ]
      BFsales[ , Age := ordered( Age, levels = sort( unique( Age ) ) ) ]

      return( BFsales )
    })


    
    output$productCategories <- renderUI({
      selectInput( inputId = "prodCats",
                   label = "Product categories: ",
                   choices = levels( getData()$Product_Category_1 ) )
    })
    
    
    
    
    getDataSubset <- reactive({
      purchase_agr <- aggregate( Purchase ~ User_ID + Age + City_Category, data = getData(), FUN = sum )
      purchase_agr <- aggregate( Purchase ~ Age + City_Category, data = BFsales, FUN = mean )
      return( purchase_agr )
    })


    output$dataDescr <- renderTable({
      print( "CREATING TABLE OF SUBSET" )
      getDataSubset() %>% head
    })

  
    
    
    output$linePlot <- renderPlot({
      print( "DRAWING PLOT" )

      ggplot( getDataSubset(), 
              aes( x = City_Category, y = Purchase, group = Age, color = Age ) ) + 
        geom_point( size = 2.5 ) +
        geom_line( lwd = 1.5 ) +
        scale_color_viridis_d( direction = -1, begin = 0.20, end = 0.85, option = "B" ) +
        labs( x = "City category",
              color = "Age band" ) +
        ggtitle( "Average spend according to customer age and city type",
                 subtitle = "- Add note here -" )
      
    })


    output$info <- renderText({
      print( "GETTING SAMPLE SIZE" )
      paste( "No. of data points:", nrow( getDataSubset() ) )
    })
    
  }
)





