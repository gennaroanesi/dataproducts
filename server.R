library(shiny)
library(sqldf)
library(psych)
library(GPArotation)


# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  
  wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",")
  names(wine)=c("ID","Alcohol","Malic_Acid","Ash","Alcalinity_of_ash","Magnesium","Total_phenols","Flavanoids","Nonflavanoid_phenols","Proanthocyanins","Color_intensity","Hue","OD280_OD315_of_diluted_wines","Proline")

  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output expressions defined 
  # below then all used the value computed from this expression
  data <- reactive({ 
		input
	})
	
  x <- reactive({ sqldf(paste("select ",paste(as.character(gsub(" ","_",input$'custom-headers')), collapse=", ")," from wine",sep=" ")) })
  
  data <- reactive({
    dist <- switch(input$a,
                   'Principal Components Analysis' = principal,
                   'Exploratory Factor Analysis'   = fa)
    
    dist(x(),nfactors=input$n,rotate=input$b)
  })

  
  # Generate a summary of the data
  output$summary <- renderPrint({
    cat("Analysis summary:")
	cat("\n Number of factors: ", input$n)
	cat("\n Variables in the model: ", paste(as.character(input$'custom-headers'), collapse=", "))
	cat("\n Analysis Type:", input$a)
	cat("\n Rotation Type:", input$b)
	cat("\n \n")
	summary(data())
  })
  
    output$loadings <- renderPrint({
    loadings(data())
  })
  
 
  output$plot <- renderPlot({
    
    biplot(data(), 
         main=paste(input$a,' for wine dataset with ',input$n,' factors', sep=''))
  })
  # Generate an HTML table view of the data

  output$table <- renderTable({
    head( x() , n = 20)
  })
  
})
"
if(input$an.type=='Principal Components Analysis'){
		
	analysis=principal(x,nfactors=input$n,rotate=input$rot.type)
	
	}
	
	if(input$an.type=='Exploratory Factor Analysis'){
	
	analysis=fa(x,nfactors=input$n,rotate=input$rot.type)
	
	}"
	
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
	
	
	