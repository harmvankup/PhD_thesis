library(tidyverse)
library(shiny)

datatypes <- c("constant formation factor and porosity"," both formation factor and porosity with depth","only porosity data with depth","only formation factor data with depth")

################################   UI   ##############################

ui <- fluidPage(
  fileInput("ConVsDepth","Concentration vs depth csv", accept = c(".csv")),
  fileInput("FormationFactorVsDepth","FF vs depth csv", accept = c(".csv")),
  fileInput("PorosityVsDepth","porosity vs depth csv", accept = c(".csv")),
  fileInput("TempVsDepth","Temperature vs depth csv", accept = c(".csv")),
  radioButtons("Datatype","What data are avaialble?",choiceNames = datatypes, choiceValues = c(0,1,2,3)),
  checkboxInput("choicediffusivity","diffusivity change with depth",value = FALSE),
  checkboxInput("compactionchoice","Compaction is considered",value = FALSE),
  textInput("name", "What's your name?"),
  numericInput("FF", "Constant formation factor", value = 1),
  numericInput("Por", "Constant porosity", value = 0),
  numericInput("Diff","Diffusivity", value = 0),
  numericInput("Temp","Known Temperature in C", value = 20),
  numericInput("sedimentationrate","Sedimentation rate in m/yr", value = 0),
  numericInput("porosity_reference","Enter the porosity at the depth where compaction becomes negligible", value = 0),
  numericInput("flow","Enter the external flow advection velocity near the seafloor in m/yr", value = 0),
  numericInput("afa","significance level for F test", value = 0.05),
  
  
  textOutput("test"),
  tableOutput("table"),
  plotOutput("plot", width = "400px")
)

####################################### server #####################################################
server <- function(input, output, session) {

  convsdepth <- reactive({
    req(input$ConVsDepth)
    
    ext <- tools::file_ext(input$ConVsDepth$name)
    switch(ext,
           csv = vroom::vroom(input$ConVsDepth$datapath, delim = ","),
           tsv = vroom::vroom(input$ConVsDepth$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv")
    )
  })
  
  FormationFactorVsDepth <- reactive({
    req(input$ConVsDepth)
    as.matrix( vroom::vroom(input$FormationFactorVsDepth$datapath, delim = ",") )
    })
  PorosityVsDepth <- reactive({
    req(input$PorosityVsDepth)
   as.matrix( vroom::vroom(input$PorosityVsDepth$datapath, delim = ",") )
  })
  
  
  
  depthdata <- reactive({
    req(input$ConVsDepth)
    
    as.matrix(convsdepth())[,1]
  })
  
  Length <- reactive({
    length(depthdata())
  })
  
  Condata <- reactive({
    req(input$ConVsDepth)
    
   as.matrix( rename_at(convsdepth()[,2], 1, ~ "concentration" ) )
  })
  
  FFdepth <- reactive({
    if(input$Datatype == 1){
       FormationFactorVsDepth()[, 1]
    } else if (input$Datatype == 2) {
       PorosityVsDepth()[, 1]
    } else if (input$Datatype == 3) {
       FormationFactorVsDepth()[, 1]
    } else if (input$Datatype == 0) {
       depthdata()
    }
  })
  
  FFdata <- reactive({
    if(input$Datatype == 1){
      FormationFactorVsDepth()[, 2]
    } else if (input$Datatype == 2) {
       10^(.1916) * PorosityVsDepth()[, 2]^(-1.8812)
    } else if (input$Datatype == 3) {
       FormationFactorVsDepth()[, 2]
    } else if (input$Datatype == 0) {
       rep(input$FF, length(depthdata()))
    }
  })
  
  Pordepth <- reactive({
    if(input$Datatype == 1){
       PorosityVsDepth()[, 1]
    } else if (input$Datatype == 2) {
      PorosityVsDepth()[, 1]
    } else if (input$Datatype == 3) {
      FFdepth()
    } else if (input$Datatype == 0) {
      depthdata()
    }
  })
  
  Pordata <- reactive({
    if (input$Datatype == 0) {
      rep(input$Por, length(depthdata()))
    } else if(input$Datatype == 1){
       PorosityVsDepth()[, 2]
    } else if (input$Datatype == 2) {
     PorosityVsDepth()[, 2]
    } else if (input$Datatype == 3) {
       10^((log10(FFdata()) - .1916) / (-1.8812))
    } 
  })
  
 Temperature0 <- reactive({input$Temp})
 
 TempVsdepth <- reactive({
   req(input$TempVsDepth)
   vroom::vroom(input$TempVsDepth$datapath, delim = ",")[,"Depth","Temperature"]
 })
  
  Temperature  <- reactive({
    req(input$ConVsDepth)
    if (input$choicediffusivity == FALSE) {
      rep(1, Length())
    } else {
       approx(TempVsdepth()[1], TempVsdepth()[2],depthdata(), method = "linear", rule = 2, f = 0, ties = "average")
    }
  })
  
  viscosity0 <- reactive({
    if (input$choicediffusivity == FALSE) {
      rep(1, Length())
    } else {
     
      if (0 <= Temperature0() & Temperature0() < 20) {
         10^((1301 / (998.333 + 8.1855 * (Temperature0() - 20) + 0.00585 * (Temperature0() - 20)^2)) - 3.30233)
      } else if (20 < Temperature0() & Temperature0() <= 100) {
         1.002 * 10^((1.3272 * (20 - Temperature0()) - 0.001053 * (Temperature0() - 20)^2) / (Temperature0() + 105))
      } else if (Temperature0() == 20) {
        1.002
      } else {
        cat("\tTemperature exceeds calculation range (0-100 celcius)\n")
      }
    } })
  
 viscosity <-  reactive({
    if (input$choicediffusivity == FALSE) {
      rep(1, length(depthdata))
    } else {
      
      
      Temperature() %>% mutate(viscosity = case_when(
        0 <= Temperature & Temperature < 20 ~ 10^((1301 / (998.333 + 8.1855 * (Temperature - 20) + 0.00585 * (Temperature - 20)^2)) - 3.30233),
        20 < Temperature & Temperature <= 100 ~ 1.002 * 10^((1.3272 * (20 - Temperature) - 0.001053 * (Temperature - 20)^2) / (Temperature + 105)),
        Temperature == 20 ~ 1.002,
        T ~ cat("\tTemperature exceeds calculation range (0-100 celcius)\n")
      ))
      
      }
  })
  
  Diffusivity <- reactive({
    if (input$choicediffusivity == FALSE) {
      rep(input$Diff, Length())
    } else {
      viscosity() %>%  mutate(Diffusivity = (Temperature+273.5) / viscosity * viscosity0() * input$Diff / (input$Temp + 273.5)) %>% select(Diffusivity)
    }
  })
  
 
  padfunction <- function(Data){
    
  earlypad <- rev(Data[1:3])
  earlypad <-Data[1] + (Data[1] - earlypad)
  
  latepad <- rev(Data[(length(Data)-2):length(Data)])
  latepad <- Data[length(Data)] + (Data[length(Data)] - Data)
  
  padded <- c(earlypad[1:2], Data, latepad[2:3])
  smoothed <-  padded %>% as.tibble() %>%  rename_at(1, ~"concentration" )  %>%  
    transmute(smoothed = lag(concentration,n = 2) * 0.06 + 0.24 * lag(concentration,n = 1) + 0.4 * concentration + 0.24 * lead(concentration,1) + 0.06 * lead(concentration,2)) %>% 
    filter(!is.na(smoothed)) %>% as.matrix()
  return(smoothed)
  }

  
  smoothedCondata <- reactive({
  padfunction(Condata())
})
  
  smoothedFFdata <- reactive({
    if(input$Datatype==0){
      FFdata()
    }
    else{
      padfunction(FFdata())
    }
  }) 
  
  smoothedPordata <- reactive({
    if(input$Datatype==0){
      Pordata()
    }
    else{
      padfunction(Pordata())
    }
  }) 
  
  
  layerfn <- function(Data){
    layer <- mean(diff(Data)) / 5 
    layercheck <- 0
    while (layercheck == 0) {
      if (layer >= 0.5) {
        layer <- layer / 5
      } else {
        layercheck <- 1
      }
    }
    return(layer)
  }
   # measurement depths of concentrations are considered
  layer <- reactive({layerfn(depthdata())})
  
  interpdepth <- reactive({seq(min(depthdata()), max(depthdata()), by = layer())})
  interpfn <- function(depth,data){
    x <- depth
    y <- data
    interpdata <- approx(x, y, xout = interpdepth(), method = "linear", rule = 2)$y
    return(interpdata)
  }
  interpFFdata <- reactive({
    interpfn(FFdepth(),smoothedFFdata())
  })
  interpPordata <- reactive({
    interpfn(Pordepth(),smoothedPordata())
  })
  interpCondata <- reactive({
    interpfn(depthdata(),smoothedCondata())
  })  
  interpDiffusivity <- reactive({
    interpfn(depthdata(),Diffusivity())
  })
  
 interpburial <- reactive({ if (Datatype == 0 | input$compactionchoice == F){
    input$sedimentationrate
  } else {
 input$porosity_reference * (1 - smoothedPordata()[1]) * input$sedimentationrate / (1 - input$porosity_reference)
  }
   })
 interpw <- reactive({
   input$flow* smoothedPordata()[1]
 })
  
 
 
  Test <- reactive({ 
    Pordata()[1:3]
    })

  #### Outputs ####
  
output$test <- renderText({
  paste0("Hello ", interpburial())
})

output$table <-  renderTable(interpburial())

output$plot <- renderPlot({
  plot(Condata(), depthdata(), col = 'blue', type = 'l')
  points(Condata(), depthdata(), col = 'red', pch = 'o')
  points(smoothedCondata(), depthdata(), col = 'black', pch = 'x')
  axis(2, at = depthdata(), labels = FALSE)
  xlabel <- 'Concentration'
  ylabel <- 'Depth'
  title(xlab = xlabel, ylab = ylabel)
  }, res = 96)

}



shinyApp(ui, server)
