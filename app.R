
# Packages ----------------------------------------------------------------

options( scipen = 999 )

library( data.table )
library( ggplot2 )
library( tidyr )



shinyApp(
  
  
  
  # UI ----------------------------------------------------------------------
  
  ui = fluidPage(
    
    titlePanel( tagList( icon( "shopping-basket" ), "Black Friday sales" ) ),
    br(),
    sidebarLayout(
      
      sidebarPanel( width = 3,
                    
                    checkboxGroupInput( inputId = "Gender", 
                                        label = "Select customer gender:", 
                                        choices = c( "Male", "Female" ), 
                                        selected = c( "Male", "Female" ) ),
                    
                    br(),
                    
                    sliderInput( inputId = "YearsSpentInCity", 
                                 label = "Number of years lived in city:",
                                 min = 0, max = 4, 
                                 value = c( 0, 4 ) ),
                    
                    br(),
                    
                    radioButtons(
                      inputId = "splitByProdCat",
                      label = "Subset by product category?",
                      choices = c( "No" = "No",
                                   "Yes" = "Yes" ),
                      selected = "No" ),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "input.splitByProdCat == 'Yes'",
                      uiOutput( "productCategories" )      # show all product categories
                    )
                    
                    
      ),
      
      mainPanel ( width = 9,
                  
                  h2( "Line graph" ),
                  plotOutput( outputId = "linePlot", width = 640 ),
                  br(),
                  h3( "My table" ),
                  tableOutput( outputId = "dataDescr" ),
                  br(),
                  h4( "Sample size" ),
                  verbatimTextOutput( outputId = "info" )
                  
      )
    )
  ),
  
  
  
  
  # Server ------------------------------------------------------------------
  
  
  
  server = function( input, output ) {
    
    
    prepData <- reactive({
      
      print( "PREPPING DATA ON START" )
      
      BFsales <- fread( "/home/caterina/Documents/TDL_PrivateRepos/BlackFridayShinyApp/BlackFriday.csv" )
      
      BFsales[ , User_ID := as.factor( User_ID ) ]
      BFsales[ , Product_ID := as.factor( Product_ID ) ]
      BFsales[ , Occupation := as.factor( Occupation ) ]
      
      BFsales[ , Gender := as.factor( Gender ) ]
      levels( BFsales$Gender ) <- c( "Female", "Male" )
      
      BFsales[ , Stay_In_Current_City_Years := ifelse( Stay_In_Current_City_Years == "4+", "4", Stay_In_Current_City_Years ) ]
      BFsales[ , Stay_In_Current_City_Years := ordered( Stay_In_Current_City_Years, 
                                                        levels = sort( unique( Stay_In_Current_City_Years ) ) ) ]
      
      BFsales[ , Marital_Status := factor( Marital_Status ) ]
      levels( BFsales$Marital_Status ) <- c( "Married", "Single" )
      
      BFsales[ , Product_Category_1 := as.factor( Product_Category_1 ) ]
      BFsales[ , Product_Category_2 := as.factor( Product_Category_2 ) ]
      BFsales[ , Product_Category_3 := as.factor( Product_Category_3 ) ]
      
      BFsales[ , Age := ifelse( Age == "0-17", "Under 17", Age ) ]
      BFsales[ , Age := ifelse( Age == "55+", "Over 55", Age ) ]
      BFsales[ , Age := ordered( Age, levels = c( "Under 17", "18-25", "26-35",  "36-45", "46-50", "51-55", "Over 55" ) ) ]
      
      return( BFsales )
      
    })
    
    
    
    
    
    output$productCategories <- renderUI({
      selectInput( inputId = "prodCats",
                   label = "Select product category: ",
                   choices = levels( prepData()$Product_Category_1 ),
                   selected = min( as.numeric( prepData()$Product_Category_1 ) ) )
    })
    
    
    
    
    
    
    
    subsetData <- reactive({
      print( "GENERATING DATA SUBSET BASED ON INPUTS" )
      
      if ( input$splitByProdCat == 'No' ) {
        selected_subset <- prepData()
      }
      else if ( input$splitByProdCat == 'Yes' ) {
        validate( need( ! is.null( input$prodCats ), "Please wait. Generating dynamic menu from data..." ) )
        selected_subset <- prepData()[ Product_Category_1 == input$prodCats, ]
      }

      
      selected_subset <- selected_subset[ Gender %in% input$Gender, ]
      
      selected_subset <- selected_subset[ input$YearsSpentInCity[ 1 ] <= Stay_In_Current_City_Years & 
                                            Stay_In_Current_City_Years <= input$YearsSpentInCity[ 2 ], ]

      return( selected_subset )
      
    })
    
    
    
    getAverageSpend <- reactive({
      purchase_agr <- aggregate( Purchase ~ User_ID + Age + City_Category, data = subsetData(), FUN = sum )
      purchase_agr <- aggregate( Purchase ~ Age + City_Category, data = purchase_agr, FUN = mean )
      return( purchase_agr )
    })
    
    
    
    output$dataDescr <- renderTable({
      
      print( "CREATING TABLE OF SUBSET" )
      tabular_vals <- dcast( Age ~ City_Category, value.var = "Purchase", data = getAverageSpend() )
      setDT( tabular_vals )
      tabular_vals[ , A := format( A, nsmall = 2, big.mark = "," ) ]
      tabular_vals[ , B := format( B, nsmall = 2, big.mark = "," ) ]
      tabular_vals[ , C := format( C, nsmall = 2, big.mark = "," ) ]
      
      setnames( tabular_vals, c( "Age band", "City A", "City B", "City C" ) )
      
      return( tabular_vals )
      
    }, align = "r" )
    
    
    
    
    output$linePlot <- renderPlot({
      print( "DRAWING PLOT" )
      
      ggplot( getAverageSpend(), 
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
      
      sample_size_per_city_type <- aggregate( User_ID ~ City_Category, data = subsetData(), FUN = function( x ) length( unique( x ) ) )
      A <- sample_size_per_city_type[[ 2 ]][ 1 ]
      B <- sample_size_per_city_type[[ 2 ]][ 2 ]
      C <- sample_size_per_city_type[[ 2 ]][ 3 ]
      
      paste( "Total number of customers for each city type (under input conditions defined): \n",
             "A =", format( A, big.mark = "," ), "\n",
             "B =", format( B, big.mark = "," ), "\n",
             "C =", format( C, big.mark = "," ) )

    })
    
  }
)





