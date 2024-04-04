library(bslib)
library(shinyChakraSlider)
library(shinyjs)
DR <- 0.02

CRF <- 0.111326528



ui <- fluidPage(
  tags$head(includeHTML(("www/google-tag4.Rhtml"))),
  useShinyjs(),
  theme = bs_theme(version = 4, bootswatch = "lux"),
 
  sidebarLayout(
    sidebarPanel( style = "overflow-y:scroll;position:relative;max-height:1200px",
  
      
      
     
      h5('Simulation Parameters'),
      
      hr(),
      sliderInput("years", "Years",
                  min = 1, max = 100, value = 10),
     
      sliderInput("nsim", "Number of Simulations",
                  min = 10, max = 1000000, value = 10000),
      
      numericInput("DR", "Discount Rate (from 0 - 1, default = 2%)",
                   min = 0, max = 1, value = .02),
      hr(),
      h5("Spillway Characteristics and Estimated Construction Costs"),
      ("This category is where you will input your barrier construction costs. If itemized estimates are availible for each category, you can change the ipnuts accordingly.
       If itemized estimates are not availible, or if categories do not apply due to differences in barrier type, select 'No' for itemized esimtates, and enter an estimated total of 
       construction costs. If you select 'No', no inputs will be considered until you reach the 'On-going Costs' section below."),
      hr(),
      selectInput("totalcost", "Itemized Estimates?",
                  c("Yes", "No")),
     # actionButton("btn", "Click to show"),
      #actionButton("btn1","Click to hide"),
     hidden(
       tags$div(
         id="conc", numericInput('costestimate', "Construction Cost Estimate (if no itemized estimate availible)", value=50000))),
     hidden(
       tags$div(
         id="itemized", 
                     numericInput("length", "Spillway Length",
                  min = 0, max = 2000000000, value = 62),
      
      
     numericInput("mobcost", "Mobilization",
                min = 0, max = 9000000000, value = 7000),
      
      numericInput("gradecost", "Grading",
                  min = 0, max = 500000000, value = 5600),
      
      numericInput("volume", "Spillway Concrete Volume",
                  min = 0, max = 2000000000, value = 48),
      
      numericInput("matcost", "Material Cost (Per Linear Meter)",
                  min = 0, max = 8000000000, value = 400),
      
      
      numericInput("conccost", "Concrete Cost (Per Cubic Meter)",
                  min = 0, max = 8000000000, value = 260),
      
      
      numericInput("engcost", "Engineering (cost per hour)",
                  min = 0, max = 2000000000, value = 80),
      
      numericInput("engtime", "Engineering time required",
      
                          min = 0, max = 20000000000, value = 91),
    #new
     )),
      
    hr(),
      h5("On-going Barrier Maintenance and Upkeep Costs"),
    ("In this section, you will input values for on-going barrier costs. Barrier maintenance is an estimated cost of annula maintenance, which will be drawn from a uniform distribution each year (+/- 200).
     Repair probability controls the probability that a repair cost will be added to the barrier maintenance cost each year. If a repair is needed, a repair cost is drawn from a uniform distribution (+/- 200)."),
    hr(),
      numericInput("minmaint", "Barrier Maintenance (min)",
                  min = 0, max = 8000000000, value = 200),
    numericInput("maxmaint", "Barrier Maintenance (max)",
                 min = 0, max = 8000000000, value = 300),
      sliderInput("repprob", "Repair Probability",
                  min = 0, max = 1, value = .6),
      
      numericInput("minrepcost", "Repair Cost (min)",
                  min = 0, max = 18000000000000, value = 800),
    numericInput("maxrepcost", "Repair Cost (max)",
                 min = 0, max = 18000000000000, value = 1200),
    numericInput("ef", "Barrier Efficiency",
                min = 0, max = 1, value = .99),
    hr(),
      
      h5("Escapement Parameters"),
("In this section, you will input values for escaped fish species of interest. You may choose to enter text labels for each species, which can help to keep track of results.
 You may choose to look at species indvidually, and/or combined."),
hr(),
    
      h5("Species 1"),
    textInput("Species1", "Species 1 (text label):"),
sliderInput("yrclass", "Number of Year classes",
            min = 1, max = 4, value = 4),
numericInput("numstock1", "Number Stocked (Species 1)", value = 300, min = 0, max = Inf),
        numericInput("ppf1", "Price Per Fish (Species 1)", value = 24, min = 0, max = 1000000),
      numericInput("pss1L", "Post Stocking Survival (low) (Species 1)", min = 0, max = 1, value = .15),
numericInput("pss1H", "Post Stocking Survival (high) (Species 1)", min = 0, max = 1, value = .35),
     numericInput("adsurv1L", "Adult Survival (low) (Species 1)", min = 0, max = 1, value = .73),
numericInput("adsurv1H", "Adult Survival (high) (Species 1)", min = .0, max = 1, value = .93),
      numericInput("ageesc1", 'First Age at Escapement (Species 1) ', min = 1, max = 50, value = 3),
hidden(
  tags$div(
    id="age2", numericInput("ageesc2", 'Second Age at Escapement (Species 1) ', min = 1, max = 50, value = 4))),
hidden(
  tags$div(
    id="age3", numericInput("ageesc3", 'Third Age at Escapement (Species 1) ', min = 1, max = 50, value = 5))),
hidden(
  tags$div(
    id="age4", numericInput("ageesc4", 'Fourth Age at Escapement (Species 1) ', min = 1, max = 50, value = 6))),

      numericInput("escapement1L", "Escapement Rate (Species 1, Low End)", min = 0, max = 1, value = .18),
      numericInput("escapement1H", "Escapement Rate (Species 1, Upper End)", min = 0, max = 1, value = .54),
      
      
      h5("Species 2"),
    textInput("Species2", "Species 2 (text label):"),
numericInput("numstock2", "Number Stocked (Species 2)", min = 1, max = Inf, value = 4500),
      
numericInput("ppf2", "Price Per Fish (Species 2)", min = .0000001, max = Inf, value = 4),
      

numericInput("pss2L", "Post Stocking Survival (low) (Species 2)", min = 0, max = 1, value = .6),
numericInput("pss2H", "Post Stocking Survival (high) (Species 2)", min = 0, max = 1, value = .8),
numericInput("adsurv2L", "Adult Survival (low) (Species 2)", min = 0, max = 1, value = .4),
numericInput("adsurv2H", "Adult Survival (high) (Species 2)", min = 0, max = 1, value = .6),







sliderInput("yrclass2", "Number of Year classes",
            min = 1, max = 4, value = 4),
numericInput("ageescsp2a1", 'First Age at Escapement (Species 2) ', min = 1, max = 5000, value = 3),
hidden(
  tags$div(
    id="ages2",numericInput("ageescsp2a2", 'Second Age at Escapement (Species 2) ', min = 1, max = 10000, value = 4))),
hidden(
  tags$div(
    id="ages3", numericInput("ageescsp2a3", 'Third Age at Escapement (Species 2) ', min = 1, max = 10000, value = 5))),
hidden(
  tags$div(
    id="ages4", numericInput("ageescsp2a4", 'Fourth Age at Escapement (Species 2) ', min = 1, max = 10000, value = 6))),     
      numericInput("escapement2L", "Escapement Rates (Species 2, Low End)", min = 0, max = 1, value = .22),
      numericInput("escapement2H", "Escapement Rates (Species 2, Upper End)", min = 0, max = 1, value = .47)

      
    ),


    mainPanel(
      tabsetPanel(
        tabPanel("Plot and Summary Statistics",
                 h4("Economic evaluation of barriers to minimize escapement of reservoir sportfishes"),
                 h6("How to use the app:"),
                 htmlOutput('HowTo'), 
                 hr(),
                 h5("Cost of Barrier vs. Escapement (2 species)"),
                 (plotOutput("PlotTwoSpecies")),
                 (dataTableOutput("TwoSpSum")),
                 
                 (textOutput("esc23")),
                 downloadButton("downloadData", "Download Barrier Data"),
                 downloadButton("downloadData2", "Download Escapement Data"),
                 
                 hr()),
        tabPanel("Plot and Summary Statistics - Species 1",
                 
                 h5(textOutput('sp1label')),
                 (plotOutput("plotfinalsp1")),
                 p("Figure 2. Plot of cost scenario for species 1 only. The default settings reflect parameters based on adult Muskellunge in Brushy Creek Lake, IA. "),
                 (dataTableOutput("summarysp1")),
                 (dataTableOutput('meansp1')),
                 (dataTableOutput('meansp1dt')),
              
                 hr(),
                 (textOutput("escsp1")),
                 hr(),
                 downloadButton("downloadData3", "Download Escapement Data for Species 1 Only"),
                 hr()), 
        tabPanel("Plot and Summary Statistics - Species 2",
                 h5(textOutput('sp2label')),
                 (plotOutput("plotfinalsp2")),
                 p(strong("Figure 3. Plot of cost scenario for species 2 only. The default settings reflect parameters based on adult Walleye in Brushy Creek Lake, IA.")),
                 (dataTableOutput("summarysp2")),
                 (dataTableOutput('meansp2')),
                 (dataTableOutput('meansp2esc')),
                 hr(),
                 (textOutput("escsp2")),
                 hr(),
                 downloadButton("downloadData4", "Download Escapement Data for Species 2 Only")),
                
      
        tabPanel("Barrier Cost Table",
                 (dataTableOutput("testvec"))),
     
        tabPanel("Escapement Cost Table",
                 (dataTableOutput("esc"))),
                 #(dataTableOutput("esc2")),
                 #(textOutput('meansp1')),
                 #(textOutput('meansp2'))),
        
        tabPanel("Barrier Pictures",
                 h5("Brushy Creek spillway before barrier installation"),
                 (img(src='Brushy_Before.jpg',height="90%", width="90%" )),
                 hr(),
                 h5('Brushy Creek barrier during construction'),
                 (img(src='Brushy_Construction.jpg',height="90%", width="90%" )),
                 hr(),
                 (img(src='Brushy_Construction_2.jpg',height="90%", width="90%" )),
                 hr(),
                 h5('Completed Brushy Creek barrier'),
                 (img(src='Brushy_Barrier.jpg',height="90%", width="90%" )),
                 hr(),
                 h6('Photos from Ben Dodd / Iowa DNR')
          
                 ),
        tabPanel("Instructions & Contact",
                 hr(),
                 ('For a comprehensive guide on how to use the application, please download the PDF below'),tags$br(),
                 downloadButton("downloadData_HowTo", "Download instructions for application"),
                 hr(),
                 ('To download metadata for all raw downloads availible in the app, please click the download button below:'),tags$br(),
                 downloadButton("downloadData_Meta", "Download metadata file"),
                 hr(),
                 'For further questions or comments about the application, please contact Madeline Lewis (mlewis2@iastate.edu) '),
        tabPanel("Model Inputs & Mechanics",
                 h5("Calcuating the cost of escapement"),

           (htmlOutput('EscCostText')),
                 (img(src='EscapementModel3.png', width='900', height='550')),
           h6('Equation list'),
           (htmlOutput('equation1')),
           (img(src='eq1.png', width='175', height='35')),
           (htmlOutput('equation2')),
           (img(src='eq2.png', width='175', height='35')),
           (htmlOutput('equation3')),
           (img(src='eq3.png', width='100', height='30')),
           h5("Calcuating the cost of the barrier"),
           (img(src='BarrierModel.png', width='500', height='300')))

      )
      
    )))






server <- function(input, output, session) {
 
  observeEvent(input$totalcost, {
    
    if(input$totalcost == 'Yes'){
      shinyjs::show(id = "itemized")
    }else{
      shinyjs::hide(id = "itemized")
    }
  })
  #observeEvent(input$totalcost == 'Yes', {show("itemized")})
 # observeEvent(input$totalcost=='No',{hide("itemized")})
  
  observeEvent(input$totalcost, {
    
    if(input$totalcost == 'No'){
      shinyjs::show(id = "conc")
    }else{
      shinyjs::hide(id = "conc")
    }
  })
  
  
  
  observeEvent(input$yrclass, {
    
    if (input$yrclass > 1 ){
      shinyjs::show(id = "age2")
 }else{
      shinyjs::hide(id = "age2")
    }
  })
  
  observeEvent(input$yrclass, {
    
    if (input$yrclass > 2 ){
      shinyjs::show(id = "age3")
    }else{
      shinyjs::hide(id = "age3")
    }
  })
  
  observeEvent(input$yrclass, {
    
    if (input$yrclass > 3 ){
      shinyjs::show(id = "age4")
    }else{
      shinyjs::hide(id = "age4")
    }
  })
  
  
  
  
  
  observeEvent(input$yrclass2, {
    
    if (input$yrclass2 > 1 ){
      shinyjs::show(id = "ages2")
    }else{
      shinyjs::hide(id = "ages2")
    }
  })
  
  observeEvent(input$yrclass2, {
    
    if (input$yrclass2 > 2 ){
      shinyjs::show(id = "ages3")
    }else{
      shinyjs::hide(id = "ages3")
    }
  })
  
  observeEvent(input$yrclass2, {
    
    if (input$yrclass2 > 3 ){
      shinyjs::show(id = "ages4")
    }else{
      shinyjs::hide(id = "ages4")
    }
  })
  
  
  
  
  
  
  yearsin <- reactive({
    years <- input$nyears
    
  })
  
  
  
  engineering <- reactive({input$engcost * input$engtime})
  
  #Mobilization and grading - doesnt change with size
  set <- reactive({input$mobcost + input$gradecost})
  
  #Material and concrete - changes with size
  scaled<- reactive({(input$matcost*input$length)+(input$volume * input$conccost)})
  
  
  upfront <- reactive({
    upfront <- (if (input$totalcost == 'Yes')
    {((input$engcost * input$engtime)+(input$mobcost + input$gradecost)+(input$matcost*input$length)+(input$volume * input$conccost))}
    else
    {input$costestimate})
    upfront <- c(rep(upfront, input$nsim))
    
  })
  
  ef <- reactive({ef = c(rep(input$ef, input$nsim))})
  
  final2 <- reactive({
    maintdata <- as.data.frame(replicate(input$years, ((runif(input$nsim, min=input$minmaint, max=input$maxmaint)))))
5
    
    maintdata$SimNum = c(seq(1,input$nsim, by=1))
    
    
    maintdata$mainsum <- rowSums(maintdata[c(1:input$years)])
    
    #Adjust for present value
    
    maintdata <- maintdata %>% mutate(mainsumPV = (mainsum / (input$years)) * (((1+DR)^input$years)-1)/(DR*(1+DR)^input$years), mainperyer=(mainsum / (input$years)))
    final <- maintdata  %>% select(SimNum, mainsumPV)
    
  })
  
  #Annual repair cost
  
  
  repair2 <- reactive({
    rep  <-  as.data.frame(replicate(input$years, ((runif(input$nsim, min=input$minrepcost, max=input$maxrepcost))*(rbinom(input$nsim, 1, input$repprob)))))
    
    rep$SimNum = c(seq(1,input$nsim, by=1))
    
    
    rep$repsum <- rowSums(rep[c(1:input$years)])
    
    rep <- rep %>% mutate(repsumPV = (repsum / (input$years)) * (((1+DR)^input$years)-1)/(DR*(1+DR)^input$years))
    
    repfinal <- rep  %>% select(repsumPV)
    
  })
  
  
  
  
  escapement <- reactive({
    
    #make matrix with the number stocked eahc year
    
    # c <- 1 - (1 - exp(-1*(-log(pss1)) + (-log(as)*(1-ae))))
    
    
    c <- matrix(rep(c(input$numstock1), input$nsim), ncol=input$years, nrow=input$nsim)
    D <- as.data.frame(c)
    
    ps1 <- as.data.frame(replicate(input$years,(runif(input$nsim,min=input$pss1L ,max=input$pss1H))))
    
    #how many survive to adult - this works
    adult<- mapply('*', D, ps1)
    
    #convert from annual to ins.
    insmortratea <- as.data.frame(replicate(input$years,(log((runif(input$nsim, min=input$adsurv1L, max=input$adsurv1H))))))
  
    ageatesc1 <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$ageesc1, max=input$ageesc1))-1))
    ageatesc2 <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$ageesc2, max=input$ageesc2))-1))
    ageatesc3 <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$ageesc3, max=input$ageesc3))-1))
    ageatesc4 <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$ageesc4, max=input$ageesc4))-1))
    #ageatesc3 <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$ageesc3, max=input$ageesc3))-1))
    #multiply ins. mort to # of years
    mort1 <- mapply('*', insmortratea, ageatesc1)
    mort1 <- as.data.frame(mort1)
    func <- function(x){
      return (1 - (1 - exp(x)))
    }
    
    #mort2 <- data.frame(lapply(mort,func)) - works
    mort1 <- map_df(mort1, func)
    
    adult1 <- as.data.frame(adult)
    mort1<- as.data.frame(mort1)
    #now works
    sy1 <- mapply('*', adult1, mort1)
    
    
  
    
    #make matrix of total stocking cost
    coststock <- matrix(rep(c(input$numstock1*input$ppf1), input$nsim), ncol=input$years, nrow=input$nsim)
    cs <- as.data.frame(coststock)
    sy1 <- as.data.frame(sy1)
  
    #matrix of repalcement cost
    repcost<- mapply('/', cs, sy1)
    
    rc1 <- as.data.frame(repcost, nrow=input$nsim, ncol=input$years)
    
    #matrix of repalcement cost
    #repcost2<- mapply('/', cs, sy2)
    
   # rc2 <- as.data.frame(repcost2, nrow=input$nsim, ncol=input$years)
    
   
    #matrix of repalcement cost
   # repcost3<- mapply('/', cs, sy3)
    
   # rc3 <- as.data.frame(repcost3, nrow=input$nsim, ncol=input$years)
    ######
    
    
    #draw escapement rates
    er <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$escapement1L, max=input$escapement1H))))
    er
    
    
    numesc <- mapply('*', er, sy1)            
    ne <- as.data.frame(numesc, nrow=input$nsim, ncol=input$years)
    
    costyr <- mapply('*', ne, rc1)
   
    #######2nd age######
    
    #multiply ins. mort to # of years
    yrto2 <- (ageatesc2-ageatesc1)
    mort2 <- mapply('*', insmortratea, yrto2)
    mort2 <- as.data.frame(mort2)
    func <- function(x){
      return (1 - (1 - exp(x)))
    }
    
    #mort2 <- data.frame(lapply(mort,func)) - works
    mort2 <- map_df(mort2, func)
    
    adult2 <- (sy1-ne)
    
    adult2<- as.data.frame(adult2)
    
    mort2<- as.data.frame(mort2)
    
    #now works
    sy2 <- mapply('*', adult2, mort2)
    
    sy2 <- as.data.frame(sy2)
    
    
    
    
    
    
    
    
    
    
    #seconbd sp
#sy22 <- (sy2 - ne)

sy22 <- as.data.frame(sy2)
#matrix of repalcement cost

#matrix of repalcement cost
repcost2<- mapply('/', cs, sy22)

rc2 <- as.data.frame(repcost2, nrow=input$nsim, ncol=input$years)
    numesc2 <- mapply('*', er, sy22)            
    ne2<- as.data.frame(numesc2, nrow=input$nsim, ncol=input$years)
    
    costyr2 <- mapply('*', ne2, rc2)
    
    ####third age
    
    
    
    #multiply ins. mort to # of years
    yrto3 <- (ageatesc3 - ageatesc2)
    mort3 <- mapply('*', insmortratea, yrto3)
    mort3 <- as.data.frame(mort3)
    func <- function(x){
      return (1 - (1 - exp(x)))
    }
    
    #mort2 <- data.frame(lapply(mort,func)) - works
    mort3 <- map_df(mort3, func)
    
    adult3 <- (sy2-(ne2))
    
    adult3<- as.data.frame(adult3)
    mort3<- as.data.frame(mort3)
    #now works
    sy3 <- mapply('*', adult3, mort3)
    
    sy3 <- as.data.frame(sy3)
    
    
    
    
    #third - num esc
   # sy34 <- (sy3 - (ne+ne2))
    
    
    repcost3<- mapply('/', cs, sy3)
   repcost3 <- pmax(repcost3, 0)
    numesc3 <- mapply('*', er, sy3)
    numesc3 <- pmax(numesc3, 0)
    ne3<- as.data.frame(numesc3, nrow=input$nsim, ncol=input$years)
    rc3 <- as.data.frame(repcost3, nrow=input$nsim, ncol=input$years)
    #ne3 <- pmax(ne3, 0)
    #rc3<-pmax(rc3,0)
    costyr3 <- mapply('*', ne3, rc3)
    
    
    ######four 
    
    
    #multiply ins. mort to # of years
    yrto4 <- (ageatesc4 - ageatesc3)
    mort4 <- mapply('*', insmortratea, yrto4)
    mort4 <- as.data.frame(mort4)
    func <- function(x){
      return (1 - (1 - exp(x)))
    }
    
    #mort2 <- data.frame(lapply(mort,func)) - works
    mort4 <- map_df(mort4, func)
    
    adult4 <- (sy3-(ne3))
    
    adult4<- as.data.frame(adult4)
    mort4<- as.data.frame(mort4)
    #now works
    sy4 <- mapply('*', adult4, mort4)
    
    sy4 <- as.data.frame(sy4)
    
    
    
    
    #third - num esc
    # sy44 <- (sy4 - (ne+ne2))
    
    
    repcost4<- mapply('/', cs, sy4)
    repcost4 <- pmax(repcost4, 0)
    numesc4 <- mapply('*', er, sy4)
    numesc4 <- pmax(numesc4, 0)
    ne4<- as.data.frame(numesc4, nrow=input$nsim, ncol=input$years)
    rc4 <- as.data.frame(repcost4, nrow=input$nsim, ncol=input$years)
    #ne4 <- pmax(ne4, 0)
    #rc4<-pmax(rc4,0)
    costyr4 <- mapply('*', ne4, rc4)
    #first species
    cost1 <- as.data.frame(costyr, nrow=input$nsim, ncol=input$years)
    
    cost1$total1 = rowSums(cost1[c(1:input$years)])
    cost1 <- cost1 %>% select(total1)
    cost1
    
    #cost1$meanperyear <-(cost1$total1)
    ##repalcementcost
    ##repalcementcost
    rc1$totalcost = rowSums(rc1[c(1:input$years)])
    rc1$mprsp1a1 = (rc1$totalcost / (input$years))
    
    rc1 <- rc1 %>% select(mprsp1a1)
    
    cost1 <- cbind(cost1, rc1)
    cost1
    
    ne$totalcost = rowSums(ne[c(1:input$years)])
    ne$mpresp1a1 = (ne$totalcost / (input$years))
    
    ne <- ne %>% select(mpresp1a1)
    cost1 <-  cbind(cost1, ne)
    cost1
    ######
    cost1<- cost1 %>% mutate(totalcostPV1 = (total1 / (input$years)) * (((1+DR)^input$years)-1)/(DR*(1+DR)^input$years)) %>% select(-c(total1)) %>% rename(total1 = totalcostPV1)
    
    ###second sp
    
    
    cost2 <- as.data.frame(costyr2, nrow=input$nsim, ncol=input$years)
    
    cost2$total2= rowSums(cost2[c(1:input$years)])
    cost2 <- cost2 %>% select(total2)
    cost2

    rc2$totalcost2 = rowSums(rc2[c(1:input$years)])
    rc2$mprsp1a2 = (rc2$totalcost2 / (input$years))
    
   
    rc2 <- rc2 %>% select(mprsp1a2)  %>% mutate(mprsp1a23=case_when((input$yrclass > 1)~mprsp1a2, input$yrclass == 1 ~ 0)) %>%select(-c(mprsp1a2)) %>% rename(mprsp1a2 = mprsp1a23)
    
    cost2 <- cbind(cost2, rc2)
    cost2
    
    ne2$totalcost2 = rowSums(ne2[c(1:input$years)])
    ne2$mpresp1a2 = (ne2$totalcost2 / (input$years))
    
    ne2 <- ne2 %>% select(mpresp1a2) %>% mutate(mpresp1a23=case_when((input$yrclass > 1)~mpresp1a2, input$yrclass == 1 ~ 0)) %>%select(-c(mpresp1a2)) %>% rename(mpresp1a2 = mpresp1a23)
    cost2 <-  cbind(cost2, ne2)
    cost2
    
    ##third year-class
 
    cost3 <- as.data.frame(costyr3, nrow=input$nsim, ncol=input$years)
    
    cost3$total3= rowSums(cost3[c(1:input$years)])
    cost3 <- cost3 %>% select(total3)
    cost3
    
    rc3$totalcost3 = rowSums(rc3[c(1:input$years)])
    rc3$mprsp1a3 = (rc3$totalcost3 / (input$years))
    
    rc3 <- rc3 %>% select(mprsp1a3)  %>% mutate(mprsp1a33=case_when((input$yrclass > 2)~mprsp1a3, input$yrclass < 3 ~ 0)) %>%select(-c(mprsp1a3)) %>% rename(mprsp1a3 = mprsp1a33)
    
    cost3 <- cbind(cost3, rc3)
    cost3
    
    ne3$totalcost3 = rowSums(ne3[c(1:input$years)])
    ne3$mpresp1a3 = (ne3$totalcost3 / (input$years))
    
    ne3 <- ne3 %>% select(mpresp1a3)  %>% mutate(mpresp1a33=case_when((input$yrclass > 2)~mpresp1a3, input$yrclass < 3 ~ 0)) %>%select(-c(mpresp1a3)) %>% rename(mpresp1a3 = mpresp1a33)
    cost3 <-  cbind(cost3, ne3)
    cost3
    cost3<- cost3 %>% mutate(totalcostPV3 = (total3 / (input$years)) * (((1+DR)^input$years)-1)/(DR*(1+DR)^input$years)) %>% select(-c(total3)) %>% mutate(total7=case_when((input$yrclass > 2)~totalcostPV3, input$yrclass < 3 ~ 0)) %>% rename(total3 = total7)
    
    ######
    cost2<- cost2 %>% mutate(totalcostPV2 = (total2 / (input$years)) * (((1+DR)^input$years)-1)/(DR*(1+DR)^input$years)) %>% select(-c(total2)) %>% mutate(total6=case_when((input$yrclass>1)~totalcostPV2, input$yrclass == 1 ~ 0)) %>% rename(total2 = total6)
    
    
    cost4 <- as.data.frame(costyr4, nrow=input$nsim, ncol=input$years)
    
    cost4$total4= rowSums(cost4[c(1:input$years)])
    cost4 <- cost4 %>% select(total4)
    cost4
    
    rc4$totalcost4 = rowSums(rc4[c(1:input$years)])
    rc4$mprsp1a4 = (rc4$totalcost4 / (input$years))
    
    rc4 <- rc4 %>% select(mprsp1a4)  %>% mutate(mprsp1a44=case_when((input$yrclass > 3)~mprsp1a4, input$yrclass < 4 ~ 0)) %>%select(-c(mprsp1a4)) %>% rename(mprsp1a4 = mprsp1a44)
    
    cost4 <- cbind(cost4, rc4)
    cost4
    
    ne4$totalcost4 = rowSums(ne4[c(1:input$years)])
    ne4$mpresp1a4 = (ne4$totalcost4 / (input$years))
    
    ne4 <- ne4 %>% select(mpresp1a4)  %>% mutate(mpresp1a44=case_when((input$yrclass > 3)~mpresp1a4, input$yrclass < 4 ~ 0)) %>%select(-c(mpresp1a4)) %>% rename(mpresp1a4 = mpresp1a44)
    cost4 <-  cbind(cost4, ne4)
    cost4
    cost4<- cost4 %>% mutate(totalcostPV4 = (total4 / (input$years)) * (((1+DR)^input$years)-1)/(DR*(1+DR)^input$years)) %>% select(-c(total4)) %>% mutate(total8=case_when((input$yrclass == 4)~totalcostPV4, input$yrclass < 4 ~ 0)) %>% rename(total4 = total8)
    
    
    cost5 <- cbind(cost2, cost1, cost3, cost4) 
    #cost1 <- cost3 %>% mutate(total4 = (total1+total2+total3), meanperyearesc = (meanperyearesc1+meanperyearesc2+meanperyearesc3)) %>% select(total4, meanperyearesc) %>% rename(total1 = total4)
    #cost1
    cost5
  })
  
  
  
  
  
  
  escapement2 <- reactive({
    
    #make matrix with the number stocked eahc year
    
    # c <- 1 - (1 - exp(-1*(-log(pss1)) + (-log(as)*(1-ae))))
    
    
    c <- matrix(rep(c(input$numstock2), input$nsim), ncol=input$years, nrow=input$nsim)
    D <- as.data.frame(c)
    
    ps1 <- as.data.frame(replicate(input$years,(runif(input$nsim,min=input$pss2L ,max=input$pss2H))))
    
    #how many survive to adult - this works
    adult<- mapply('*', D, ps1)
    
    #convert from annual to ins.
    insmortratea <- as.data.frame(replicate(input$years,(log((runif(input$nsim, min=input$adsurv2L, max=input$adsurv2H))))))
    
    ageatesc1 <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$ageescsp2a1, max=input$ageescsp2a1))-1))
    ageatesc2 <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$ageescsp2a2, max=input$ageescsp2a2))-1))
    ageatesc3 <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$ageescsp2a3, max=input$ageescsp2a3))-1))
    ageatesc4 <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$ageescsp2a4, max=input$ageescsp2a4))-1))
    #ageatesc3 <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$ageesc3, max=input$ageesc3))-1))
    #multiply ins. mort to # of years
    mort1 <- mapply('*', insmortratea, ageatesc1)
    mort1 <- as.data.frame(mort1)
    func <- function(x){
      return (1 - (1 - exp(x)))
    }
    
    #mort2 <- data.frame(lapply(mort,func)) - works
    mort1 <- map_df(mort1, func)
    
    adult1 <- as.data.frame(adult)
    mort1<- as.data.frame(mort1)
    #now works
    sy1 <- mapply('*', adult1, mort1)
    
    
    
    
    #make matrix of total stocking cost
    coststock <- matrix(rep(c(input$numstock2*input$ppf2), input$nsim), ncol=input$years, nrow=input$nsim)
    cs <- as.data.frame(coststock)
    sy1 <- as.data.frame(sy1)
    
    #matrix of repalcement cost
    repcost<- mapply('/', cs, sy1)
    
    rc1 <- as.data.frame(repcost, nrow=input$nsim, ncol=input$years)
    
    #matrix of repalcement cost
    #repcost2<- mapply('/', cs, sy2)
    
    # rc2 <- as.data.frame(repcost2, nrow=input$nsim, ncol=input$years)
    
    
    #matrix of repalcement cost
    # repcost3<- mapply('/', cs, sy3)
    
    # rc3 <- as.data.frame(repcost3, nrow=input$nsim, ncol=input$years)
    ######
    
    
    #draw escapement rates
    er <- as.data.frame(replicate(input$years,(runif(input$nsim, min=input$escapement2L, max=input$escapement2H))))
    er
    
    
    numesc <- mapply('*', er, sy1)            
    ne <- as.data.frame(numesc, nrow=input$nsim, ncol=input$years)
    
    costyr <- mapply('*', ne, rc1)
    
    #######2nd age######
    
    #multiply ins. mort to # of years
    yrto2 <- (ageatesc2-ageatesc1)
    mort2 <- mapply('*', insmortratea, yrto2)
    mort2 <- as.data.frame(mort2)
    func <- function(x){
      return (1 - (1 - exp(x)))
    }
    
    #mort2 <- data.frame(lapply(mort,func)) - works
    mort2 <- map_df(mort2, func)
    
    adult2 <- (sy1-ne)
    
    adult2<- as.data.frame(adult2)
    
    mort2<- as.data.frame(mort2)
    
    #now works
    sy2 <- mapply('*', adult2, mort2)
    
    sy2 <- as.data.frame(sy2)
    
    
    
    
    
    
    
    
    
    
    #seconbd sp
    #sy22 <- (sy2 - ne)
    
    sy22 <- as.data.frame(sy2)
    #matrix of repalcement cost
    
    #matrix of repalcement cost
    repcost2<- mapply('/', cs, sy22)
    
    rc2 <- as.data.frame(repcost2, nrow=input$nsim, ncol=input$years)
    numesc2 <- mapply('*', er, sy22)            
    ne2<- as.data.frame(numesc2, nrow=input$nsim, ncol=input$years)
    
    costyr2 <- mapply('*', ne2, rc2)
    
    ####third age
    
    
    
    #multiply ins. mort to # of years
    yrto3 <- (ageatesc3-ageatesc2)
    mort3 <- mapply('*', insmortratea, yrto3)
    mort3 <- as.data.frame(mort3)
    func <- function(x){
      return (1 - (1 - exp(x)))
    }
    
    #mort2 <- data.frame(lapply(mort,func)) - works
    mort3 <- map_df(mort3, func)
    
    adult3 <- (sy2-(ne2))
    
    adult3<- as.data.frame(adult3)
    mort3<- as.data.frame(mort3)
    #now works
    sy3 <- mapply('*', adult3, mort3)
    
    sy3 <- as.data.frame(sy3)
    
    
    
    
    #third - num esc
    # sy34 <- (sy3 - (ne+ne2))
    
    
    repcost3<- mapply('/', cs, sy3)
    repcost3 <- pmax(repcost3, 0)
    numesc3 <- mapply('*', er, sy3)
    numesc3 <- pmax(numesc3, 0)
    ne3<- as.data.frame(numesc3, nrow=input$nsim, ncol=input$years)
    rc3 <- as.data.frame(repcost3, nrow=input$nsim, ncol=input$years)
    #ne3 <- pmax(ne3, 0)
    #rc3<-pmax(rc3,0)
    costyr3 <- mapply('*', ne3, rc3)
    
    
    
    ####fourth
    
    
    
    #multiply ins. mort to # of years
    yrto4 <- (ageatesc4-ageatesc3)
    mort4 <- mapply('*', insmortratea, yrto4)
    mort4 <- as.data.frame(mort4)
    func <- function(x){
      return (1 - (1 - exp(x)))
    }
    
    #mort2 <- data.frame(lapply(mort,func)) - works
    mort4 <- map_df(mort4, func)
    
    adult4 <- (sy3-(ne3))
    
    adult4<- as.data.frame(adult4)
    mort4<- as.data.frame(mort4)
    #now works
    sy4 <- mapply('*', adult4, mort4)
    
    sy4 <- as.data.frame(sy4)
    
    
    
    
    #third - num esc
    # sy44 <- (sy4 - (ne+ne2))
    
    
    repcost4<- mapply('/', cs, sy4)
    repcost4 <- pmax(repcost4, 0)
    numesc4 <- mapply('*', er, sy4)
    numesc4 <- pmax(numesc4, 0)
    ne4<- as.data.frame(numesc4, nrow=input$nsim, ncol=input$years)
    rc4 <- as.data.frame(repcost4, nrow=input$nsim, ncol=input$years)
    #ne4 <- pmax(ne4, 0)
    #rc4<-pmax(rc4,0)
    costyr4 <- mapply('*', ne4, rc4)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #first species
    cost1 <- as.data.frame(costyr, nrow=input$nsim, ncol=input$years)
    
    cost1$total1 = rowSums(cost1[c(1:input$years)])
    cost1 <- cost1 %>% select(total1)
    cost1
    
    #cost1$meanperyear <-(cost1$total1)
    ##repalcementcost
    ##repalcementcost
    rc1$totalcost = rowSums(rc1[c(1:input$years)])
    rc1$mprsp2a1 = (rc1$totalcost / (input$years))
    
    rc1 <- rc1 %>% select(mprsp2a1)
    
    cost1 <- cbind(cost1, rc1)
    cost1
    
    ne$totalcost = rowSums(ne[c(1:input$years)])
    ne$mpresp2a1 = (ne$totalcost / (input$years))
    
    ne <- ne %>% select(mpresp2a1)
    cost1 <-  cbind(cost1, ne)
    cost1
    ######
    cost1<- cost1 %>% mutate(totalcostsp2PV1 = (total1 / (input$years)) * (((1+DR)^input$years)-1)/(DR*(1+DR)^input$years)) %>% select(-c(total1)) %>% rename(totalsp2a1 = totalcostsp2PV1)
    
    ###second sp
    
    cost2 <- as.data.frame(costyr2, nrow=input$nsim, ncol=input$years)
    
    cost2$total2= rowSums(cost2[c(1:input$years)])
    cost2 <- cost2 %>% select(total2)
    cost2
    
    rc2$totalcost2 = rowSums(rc2[c(1:input$years)])
    rc2$mprsp2a2 = (rc2$totalcost2 / (input$years))
    
    
    rc2 <- rc2 %>% select(mprsp2a2)  %>% mutate(mprsp2a23=case_when((input$yrclass > 1)~mprsp2a2, input$yrclass == 1 ~ 0)) %>%select(-c(mprsp2a2)) %>% rename(mprsp2a2 = mprsp2a23)
    
    cost2 <- cbind(cost2, rc2)
    cost2
    
    ne2$totalcost2 = rowSums(ne2[c(1:input$years)])
    ne2$mpresp2a2 = (ne2$totalcost2 / (input$years))
    
    ne2 <- ne2 %>% select(mpresp2a2) %>% mutate(mpresp2a23=case_when((input$yrclass > 1)~mpresp2a2, input$yrclass == 1 ~ 0)) %>%select(-c(mpresp2a2)) %>% rename(mpresp2a2 = mpresp2a23)
    cost2 <-  cbind(cost2, ne2)
    cost2
    ##third year-class
    
    cost3 <- as.data.frame(costyr3, nrow=input$nsim, ncol=input$years)
    
    cost3$total3= rowSums(cost3[c(1:input$years)])
    cost3 <- cost3 %>% select(total3)
    cost3
    
    rc3$totalcost3 = rowSums(rc3[c(1:input$years)])
    rc3$mprsp2a3 = (rc3$totalcost3 / (input$years))
    
    rc3 <- rc3 %>% select(mprsp2a3)  %>% mutate(mprsp2a33=case_when((input$yrclass2 > 2)~mprsp2a3, input$yrclass2 < 3 ~ 0)) %>%select(-c(mprsp2a3)) %>% rename(mprsp2a3 = mprsp2a33)
    
    cost3 <- cbind(cost3, rc3)
    cost3
    
    ne3$totalcost3 = rowSums(ne3[c(1:input$years)])
    ne3$mpresp2a3 = (ne3$totalcost3 / (input$years))
    
    ne3 <- ne3 %>% select(mpresp2a3)  %>% mutate(mpresp2a33=case_when((input$yrclass2 > 2)~mpresp2a3, input$yrclass2 < 3 ~ 0)) %>%select(-c(mpresp2a3)) %>% rename(mpresp2a3 = mpresp2a33)
    cost3 <-  cbind(cost3, ne3)
    cost3
    cost3<- cost3 %>% mutate(totalcostsp2PV3 = (total3 / (input$years)) * (((1+DR)^input$years)-1)/(DR*(1+DR)^input$years)) %>% select(-c(total3)) %>% mutate(total7=case_when((input$yrclass > 2)~totalcostsp2PV3, input$yrclass < 3 ~ 0)) %>% rename(totalsp2a3 = total7)
    
    ###fourth
    
    
    cost4 <- as.data.frame(costyr4, nrow=input$nsim, ncol=input$years)
    
    cost4$total4= rowSums(cost4[c(1:input$years)])
    cost4 <- cost4 %>% select(total4)
    cost4
    
    rc4$totalcost4 = rowSums(rc4[c(1:input$years)])
    rc4$mprsp2a4 = (rc4$totalcost4 / (input$years))
    
    rc4 <- rc4 %>% select(mprsp2a4)  %>% mutate(mprsp2a44=case_when((input$yrclass2 > 3)~mprsp2a4, input$yrclass2 < 4 ~ 0)) %>%select(-c(mprsp2a4)) %>% rename(mprsp2a4 = mprsp2a44)
    
    cost4 <- cbind(cost4, rc4)
    cost4
    
    ne4$totalcost4 = rowSums(ne4[c(1:input$years)])
    ne4$mpresp2a4 = (ne4$totalcost4 / (input$years))
    
    ne4 <- ne4 %>% select(mpresp2a4)  %>% mutate(mpresp2a44=case_when((input$yrclass2 > 3)~mpresp2a4, input$yrclass2 < 4 ~ 0)) %>%select(-c(mpresp2a4)) %>% rename(mpresp2a4 = mpresp2a44)
    cost4 <-  cbind(cost4, ne4)
    cost4
    cost4<- cost4 %>% mutate(totalcostsp2PV4 = (total4 / (input$years)) * (((1+DR)^input$years)-1)/(DR*(1+DR)^input$years)) %>% select(-c(total4)) %>% mutate(total7=case_when((input$yrclass == 4)~totalcostsp2PV4, input$yrclass < 4 ~ 0)) %>% rename(totalsp2a4 = total7)
    
    
    
    
    
    
    
    
    
    
    ######
    cost2<- cost2 %>% mutate(totalcostsp2PV2 = (total2 / (input$years)) * (((1+DR)^input$years)-1)/(DR*(1+DR)^input$years)) %>% select(-c(total2)) %>% mutate(total6=case_when((input$yrclass>1)~totalcostsp2PV2, input$yrclass == 1 ~ 0)) %>% rename(totalsp2a2 = total6)
    cost5 <- cbind(cost2, cost1, cost3, cost4) 
    #cost1 <- cost3 %>% mutate(total4 = (total1+total2+total3), meanperyearesc = (meanperyearesc1+meanperyearesc2+meanperyearesc3)) %>% select(total4, meanperyearesc) %>% rename(total1 = total4)
    #cost1
    cost5
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 testvec<- reactive({
    maint <- final2()
    repair <- repair2()
    upfront <- upfront()
    ef <- ef()
    esc <- escapement()
    esc2 <- escapement2()
    twospec <-cbind(esc, esc2)
    two <- twospec %>% mutate(totalcomb = (total1+total2+total3+total4+(totalsp2a1+totalsp2a2+totalsp2a2+totalsp2a4))) %>% select(totalcomb)
    twoef <- cbind(ef, two)
    
   CostEf <- twoef %>% mutate(CostEff = (totalcomb * (1-ef))) %>% select(CostEff)

    var <- cbind(maint, repair, upfront, CostEf)
    comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront+CostEff))  %>% rename('Simulation Number' = 'SimNum',
                                                                                                    'Present value of maintanence costs over X years' = 'mainsumPV',
                                                                                                    'Present value of repair costs over x years' = 'repsumPV',
                                                                                                    'Construction costs' = 'upfront', 'Total barrier cost' = 'total')
 })
  
                                                                                                                                                                              
   

 
 
 output$testvec<- renderDataTable({ testvec() })
  
  
 # output$testvec<- renderDataTable({
    
   # maint <- final2()
  #  repair <- repair2()
   # upfront <- upfront()
   # var <- cbind(maint, repair, upfront)
  #  comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront))  %>% rename('Simulation Number' = 'SimNum',
 #                                                                                                   'Present value of maintanence costs over X years' = 'mainsumPV',
 #                                                                                                   'Present value of repair costs over x years' = 'repsumPV',
  #                                                                                                  'Construction costs' = 'upfront',
     #                                                                                               'Total present value of barrier costs' = 'total')
   # round(comb, digits=2)
    
 # })
  
  
  output$TwoSpSum<- renderDataTable({
    maint <- final2()
    repair <- repair2()
    upfront <- upfront()
    
    ef <- ef()
    esc <- escapement()
    esc2 <- escapement2()
    twospec <-cbind(esc, esc2)
    
    
    #cost1 <- cost3 %>% mutate(total4 = (total1+total2+total3), meanperyearesc = (meanperyearesc1+meanperyearesc2+meanperyearesc3)) %>% select(total4, meanperyearesc) %>% rename(total1 = total4)
    #cost1
    
    two <- twospec %>% mutate(totalcomb = ((total1+total2+total3+total4)+(totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4))) %>% select(totalcomb)
    twoef <- cbind(ef, two)
    
    CostEf <- twoef %>% mutate(CostEff = (totalcomb * (1-ef))) %>% select(CostEff)
    
    
    var <- cbind(maint, repair, upfront, CostEf)
    comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront+CostEff), scenario='Barrier') %>% select(total, scenario)
    
    esc <- escapement()
    esc2 <- escapement2()
    twospec <-cbind(esc, esc2)
    two <- twospec %>% mutate(totalcomb = ((total1+total2+total3+total4)+(totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4))) 
    
    #%>% rename('Total for Species 1 - age 1' = 'total1', 'Total for Species 1 - age 2' = 'total2', 'Total for Species 1 - age 3' = 'total3', 'Total for Species 2 - age 1' = 'totalsp2a1','Total for Species 2 - age 2' = 'totalsp2a3','Total for Species 2 - age 3' = 'totalsp2a3')
    esca <- two %>% mutate(scenario='No Barrier') %>% select(totalcomb, scenario)
    
    as.data.frame(comb)
    as.data.frame(esca)
    
    comb$total = as.numeric(comb$total)
    esca$total = as.numeric(esca$totalcomb)
    cmbine <- rbind(comb, esca)

    both2<- cmbine %>% group_by(scenario) %>% summarise(mn = mean(total), min = min(total), max = max(total)) %>% mutate(across(2:4, round, 2))%>% rename('Scenario'= 'scenario', 'Minimum Cost' ='min', 'Maximum Cost' = 'max', 'Mean Cost' = 'mn')
    datatable(both2, options = list(dom = 't') )
    
  })
  
  
  output$summarysp1<- renderDataTable({
    maint <- final2()
    repair <- repair2()
    upfront <- upfront()
    ef <- ef()
    esc <- escapement()
  

    twoef <- cbind(esc, ef)
    
    CostEf <- twoef %>% mutate(CostEff = ((total1+total2+total3+total4)* (1-ef))) %>% select(CostEff)
    
    
    
    
    var <- cbind(maint, repair, upfront, CostEf)
    comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront+CostEff), scenario='Barrier') %>% select(total, scenario)
    
    esc <- escapement()
    
    esca <- esc %>% mutate(scenario='No Barrier') %>% select(total1,total2,total3, total4, scenario) %>% mutate(totale = (total1+total2+total3+total4))
    
    as.data.frame(comb)
    as.data.frame(esca)
    
    comb$total = as.numeric(comb$total)
    esca$total = as.numeric(esca$totale)
    cmbine <- rbind(comb, esca)
    
    both2<- cmbine %>% group_by(scenario) %>% summarise(mn = mean(total), min = min(total), max = max(total)) %>% mutate(across(2:4, round, 2))%>% rename('Scenario'= 'scenario', 'Minimum Cost' ='min', 'Maximum Cost' = 'max', 'Mean Cost' = 'mn')
    datatable(both2, options = list(dom = 't'))
    
  })
  
  
  output$summarysp2<- renderDataTable({
    maint <- final2()
    repair <- repair2()
    upfront <- upfront()
    ef <- ef()
    esc <- escapement2()
    
    ef <- ef()



    
    twoef <- cbind(esc, ef)
    
    CostEf <- twoef %>% mutate(CostEff = (totalsp2a1+totalsp2a2+totalsp2a3) * (1-ef)) %>% select(CostEff)
    
    
    
    
    
    
    var <- cbind(maint, repair, upfront, CostEf)
    comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront+CostEff), scenario='Barrier') %>% select(total, scenario)
    
    esc <- escapement2()
    
    esca <- esc %>% mutate(scenario='No Barrier') %>% mutate(totalsp2 = (totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4)) %>% select(totalsp2, scenario)
    
    as.data.frame(comb)
    as.data.frame(esca)
    
    comb$total = as.numeric(comb$total)
    esca$total = as.numeric(esca$totalsp2)
    cmbine <- rbind(comb, esca)
    
    both2<- cmbine %>% group_by(scenario) %>% summarise(mn = mean(total), min = min(total), max = max(total)) %>% mutate(across(2:4, round, 2))%>% rename('Scenario'= 'scenario', 'Minimum Cost' ='min', 'Maximum Cost' = 'max', 'Mean Cost' = 'mn')
    datatable(both2, options = list(dom = 't'))
    
  })
  
  
  
  esc<- reactive({
    
    esc <- escapement()
    esc2 <- escapement2()
    twospec <-cbind(esc, esc2)
    twospec
    two <- twospec %>%  mutate(totalcomb = ((total1+total2+total3+total4)+(totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4))) %>% mutate('Total Escapement Cost for Both Species' = ((total1+total2+total3+total4)+(totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4)), totalsp1 = (total1+total2+total3+total4), totalsp2 = (totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4)) %>% rename('Total for Species 1' = 'totalsp1', 'Total for Species 2' = 'totalsp2') %>% select(-c( total2, total3, total4, totalsp2a2, totalsp2a3,totalsp2a3,totalcomb, totalsp2a4 )) %>% rename('totalcostPV1' = 'total1', 'totalcostsp2PV1' = 'totalsp2a1')
    round(two, digits = 2)
    
  })
  
  output$esc<- renderDataTable({esc()})
  
  
  escsp1<- reactive({
    
    esc <- escapement()
    
  
    spec1 <- esc %>%  mutate(totalcomb = ((total1+total2+total3+total4))) %>% mutate('Total Escapement Cost for Species 1' = ((total1+total2+total3+total4))) %>% select(-c( total2, total3, total4, totalcomb )) %>% rename('totalcostPV1' = 'total1')
    round(spec1, digits = 2)
    
  })
  
  output$escsp1<- renderDataTable({escsp1()})
  
  
  
  escsp2<- reactive({
    

    esc2 <- escapement2()
   
    two <- esc2 %>%  mutate(totalcomb = (totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4)) %>% rename('Total for Species 2' = 'totalcomb') %>% select(-c( totalsp2a2, totalsp2a3,totalsp2a3, totalsp2a4 )) %>% rename( 'totalcostsp2PV1' = 'totalsp2a1')
    round(two, digits = 2)
    
  })
  
  output$escsp2<- renderDataTable({escsp2()})
  
  
  #output$esc<- renderDataTable({
    
  #  esc <- escapement()
  #  esc2 <- escapement2()
  #  twospec <-cbind(esc, esc2)
   # twospec
   # two <- twospec %>% select(total1, total2) %>% mutate('Total Escapement Cost for Both Species' = (total1+total2)) %>% rename('Total for Species 1' = 'total1', 'Total for Species 2' = 'total2')
    #round(two, digits = 2)
  
 # })
  
  output$esc23<- renderText({
    
    esc <- escapement()
    esc2 <- escapement2()
    twospec <-cbind(esc, esc2)
    twospec
    two <- twospec %>% select(total1, total2, total3, total4, totalsp2a1, totalsp2a2, totalsp2a3, totalsp2a4) %>% mutate(Totalesc = ((total1+total2+total3+total4)+(totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4)))  %>% select(Totalesc)
    #new
    maint <- final2()
    repair <- repair2()
    upfront <- upfront()
    
    ef <- ef()
   
    twospec <-cbind(esc, esc2)
    twoe <- twospec %>% mutate(totalcomb = ((total1+total2+total3+total4)+(totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4))) %>% select(totalcomb)
    twoef <- cbind(ef, twoe)
    
    CostEf <- twoef %>% mutate(CostEff = (totalcomb * (1-ef))) %>% select(CostEff)
    
    
    var <- cbind(maint, repair, upfront, CostEf)
    comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront+CostEff))  %>% select(total)
   
    new <- cbind(two, comb)
    new <- new %>% mutate(ex = (Totalesc - total)) %>% mutate(ce = if_else(ex>0, 1, 0)) %>% summarise(cb = (sum(ce)/ input$nsim)) %>% select(cb) %>% mutate(c1 = 'Probability that barrier cost < Escapement cost')
    
   # %>% rename('Probability that barrier cost < Escapement cost' = 'cb') 
prob <- new$cb

print(paste('Probability of a positive net benefit (i.e., probability that barrier cost < escapement cost = ', prob))
   
  })
  
  #Table for probability that species 1 escapement will exceed barrier cost
  output$escsp1<- renderText({
    
    esc <- escapement()
    #new
    maint <- final2()
    repair <- repair2()
    upfront <- upfront()
    
    ef <- ef()
   
    twoef <- cbind(ef, esc)
    twoef <- twoef %>% mutate(totale = ((total1+total2+total3+total4))) 
    CostEf <- twoef %>% mutate(CostEff = (totale * (1-ef))) %>% select(CostEff)
    
    
    var <- cbind(maint, repair, upfront, CostEf)
    comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront+CostEff))  %>% select(total)
    
    

    esc <- esc %>% mutate(totale = ((total1+total2+total3+total4))) 
    new <- cbind(esc, comb)
    new <- new %>% mutate(ex = (totale - total)) %>% mutate(ce = if_else(ex>0, 1, 0)) %>% summarise(cb = (sum(ce)/ input$nsim))
    new
    prob <- new$cb
    
    print(paste('Probability of a postive net benefit (i.e., probability that barrier cost < escapement cost = ', prob))
  })
  
  
  #Table for probability that species 2 escapement will exceed barrier cost
  output$escsp2<- renderText({
    
    esc <- escapement2()
    #new
    maint <- final2()
    repair <- repair2()
    upfront <- upfront()
    ef <- ef()
   
    twoef <- cbind(ef, esc)
    
    CostEf <- twoef %>% mutate(CostEff = ((totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4) * (1-ef))) %>% select(CostEff)
    
    
    var <- cbind(maint, repair, upfront, CostEf)
    comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront+CostEff))  %>% select(total)
    

    
    new <- cbind(esc, comb)
    new <- new %>% mutate(ex = ((totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4) - total)) %>% mutate(ce = if_else(ex>0, 1, 0)) %>% summarise(cb = (sum(ce)/ input$nsim))
    
    prob <- new$cb
    
    print(paste('Probability of a postive net benefit (i.e., probability that barrier cost < escapement cost = ', prob))
  })
  
  
  #Table of replacement cost for species 1
  output$meansp1<- renderDataTable({
    
    esc <- escapement()
    meansp1a1 <- mean(esc$mprsp1a1)
    minsp1a1 <- min(esc$mprsp1a1)
    maxsp1a1 <- max(esc$mprsp1a1)
    
    meansp1a2 <- mean(esc$mprsp1a2)
    minsp1a2 <- min(esc$mprsp1a2)
    maxsp1a2 <- max(esc$mprsp1a2)
    
    meansp1a3 <- mean(esc$mprsp1a3)
    minsp1a3 <- min(esc$mprsp1a3)
    maxsp1a3 <- max(esc$mprsp1a3)
   
    meansp1a4 <- mean(esc$mprsp1a4)
    minsp1a4 <- min(esc$mprsp1a4)
    maxsp1a4 <- max(esc$mprsp1a4) 
    both2 <- cbind(meansp1a1, minsp1a1, maxsp1a1, meansp1a2, minsp1a2, maxsp1a2, meansp1a3, minsp1a3, maxsp1a3,  meansp1a4, minsp1a4, maxsp1a4) 
    both2 <- as.data.frame(both2)
    #both2 <- both2 %>% rename('Mean Rep Cost' = 'meansp1', "Min Rep Cost" = 'minsp1', "Max Rep Cost"='maxsp1')
    both2 <- both2 %>% rename('Mean Replacement Cost Per Year (Sp. 1, Age 1)'= meansp1a1, 'Min Replacement Cost Per Year (Sp. 1, Age 1)'= minsp1a1, 'Max Replacement Cost Per Year (Sp. 1, Age 1)'= maxsp1a1,
                              'Mean Replacement Cost Per Year (Sp. 1, Age 2)'= meansp1a2,  'Min Replacement Cost Per Year (Sp. 1, Age 2)'= minsp1a2, 'Max Replacement Cost Per Year (Sp. 1, Age 2)'= maxsp1a2,
                              'Mean Replacement Cost Per Year (Sp. 1, Age 3)'= meansp1a3,  'Min Replacement Cost Per Year (Sp. 1, Age 3)'= minsp1a3, 'Max Replacement Cost Per Year (Sp. 1, Age 3)'= maxsp1a3,
                              'Mean Replacement Cost Per Year (Sp. 1, Age 4)'= meansp1a4,  'Min Replacement Cost Per Year (Sp. 1, Age 4)'= minsp1a4, 'Max Replacement Cost Per Year (Sp. 1, Age 4)'= maxsp1a4 )
    
    
   both2 <- both2 %>% mutate(across(1:12, round, 2))
  datatable(both2, options = list(dom = 't'))

  })
  #Table of number escaped for species 1
  output$meansp1dt<- renderDataTable({
    
    esc <- escapement()
    meanspe1a1 <- mean(esc$mpresp1a1)
    minspe1a1 <- min(esc$mpresp1a1)
    maxspe1a1 <- max(esc$mpresp1a1)
    
    meanspe1a2 <- mean(esc$mpresp1a2)
    minspe1a2 <- min(esc$mpresp1a2)
    maxspe1a2 <- max(esc$mpresp1a2)
    
    meanspe1a3 <- mean(esc$mpresp1a3)
    minspe1a3 <- min(esc$mpresp1a3)
    maxspe1a3 <- max(esc$mpresp1a3)
    
    meanspe1a4 <- mean(esc$mpresp1a4)
    minspe1a4 <- min(esc$mpresp1a4)
    maxspe1a4 <- max(esc$mpresp1a4)
    
    both2 <- cbind(meanspe1a1, minspe1a1, maxspe1a1, meanspe1a2, minspe1a2, maxspe1a2, meanspe1a3, minspe1a3, maxspe1a3,  meanspe1a4, minspe1a4, maxspe1a4)

    both2 <- as.data.frame(both2)
    
    both2 <- both2 %>% rename('Mean Number Escaped Per Year (Sp. 1, Age 1)'= meanspe1a1, 'Min Number Escaped Per Year (Sp. 1, Age 1)'= minspe1a1, 'Max Number Escaped Per Year (Sp. 1, Age 1)'= maxspe1a1,
                              'Mean Number Escaped Per Year (Sp. 1, Age 2)'= meanspe1a2,  'Min Number Escaped Per Year (Sp. 1, Age 2)'= minspe1a2, 'Max Number Escaped Per Year (Sp. 1, Age 2)'= maxspe1a2,
                              'Mean Number Escaped Per Year (Sp. 1, Age 3)'= meanspe1a3,  'Min Number Escaped Per Year (Sp. 1, Age 3)'= minspe1a3, 'Max Number Escaped Per Year (Sp. 1, Age 3)'= maxspe1a3,
                              'Mean Number Escaped Per Year (Sp. 1, Age 4)'= meanspe1a4,  'Min Number Escaped Per Year (Sp. 1, Age 4)'= minspe1a4, 'Max Number Escaped Per Year (Sp. 1, Age 4)'= maxspe1a4 )
    both2 <- both2 %>% mutate(across(1:12, round, 2))
    datatable(both2,  options = list(dom = 't') )
  })
  
  
  
  
  
  

  
  #rep cost
  
  output$meansp2<- renderDataTable({
    
    esc <- escapement2()
    meansp2a1 <- mean(esc$mprsp2a1)
    minsp2a1 <- min(esc$mprsp2a1)
    maxsp2a1 <- max(esc$mprsp2a1)
    
    meansp2a2 <- mean(esc$mprsp2a2)
    minsp2a2 <- min(esc$mprsp2a2)
    maxsp2a2 <- max(esc$mprsp2a2)
    
    meansp2a3 <- mean(esc$mprsp2a3)
    minsp2a3 <- min(esc$mprsp2a3)
    maxsp2a3 <- max(esc$mprsp2a3)
    
    meansp2a4 <- mean(esc$mprsp2a4)
    minsp2a4 <- min(esc$mprsp2a4)
    maxsp2a4 <- max(esc$mprsp2a4)
    
    
    both2 <- cbind(meansp2a1, minsp2a1, maxsp2a1, meansp2a2, minsp2a2, maxsp2a2, meansp2a3, minsp2a3, maxsp2a3, meansp2a4, minsp2a4, maxsp2a4) 
    both2 <- as.data.frame(both2)
    both2 <- both2 %>% rename('Mean Replacement Cost Per Year (Sp. 2, Age 1)'= meansp2a1, 'Min Replacement Cost Per Year (Sp. 2, Age 1)'= minsp2a1, 'Max Replacement Cost Per Year (Sp. 2, Age 1)'= maxsp2a1,
                              'Mean Replacement Cost Per Year (Sp. 2, Age 2)'= meansp2a2,  'Min Replacement Cost Per Year (Sp. 2, Age 2)'= minsp2a2, 'Max Replacement Cost Per Year (Sp. 2, Age 2)'= maxsp2a2,
                              'Mean Replacement Cost Per Year (Sp. 2, Age 3)'= meansp2a3,  'Min Replacement Cost Per Year (Sp. 2, Age 3)'= minsp2a3, 'Max Replacement Cost Per Year (Sp. 2, Age 3)'= maxsp2a3,
                              'Mean Replacement Cost Per Year (Sp. 2, Age 4)'= meansp2a4,  'Min Replacement Cost Per Year (Sp. 2, Age 4)'= minsp2a4, 'Max Replacement Cost Per Year (Sp. 2, Age 4)'= maxsp2a4 )
    both2 <- both2 %>% mutate(across(1:12, round, 2))
    datatable(both2, options = list(dom = 't') )
  })
  
  
  
  #escape
  output$meansp2esc<- renderDataTable({
    
    
    esc <- escapement2()
    meanspe2a1 <- mean(esc$mpresp2a1)
    minspe2a1 <- min(esc$mpresp2a1)
    maxspe2a1 <- max(esc$mpresp2a1)
    
    meanspe2a2 <- mean(esc$mpresp2a2)
    minspe2a2 <- min(esc$mpresp2a2)
    maxspe2a2 <- max(esc$mpresp2a2)
    
    meanspe2a3 <- mean(esc$mpresp2a3)
    minspe2a3 <- min(esc$mpresp2a3)
    maxspe2a3 <- max(esc$mpresp2a3)
    
    meanspe2a4 <- mean(esc$mpresp2a4)
    minspe2a4 <- min(esc$mpresp2a4)
    maxspe2a4 <- max(esc$mpresp2a4)
    
    both2 <- cbind(meanspe2a1, minspe2a1, maxspe2a1, meanspe2a2, minspe2a2, maxspe2a2, meanspe2a3, minspe2a3, maxspe2a3, meanspe2a4, minspe2a4, maxspe2a4) 
    both2 <- as.data.frame(both2)
    both2 <- both2 %>% rename('Mean Number Escaped Per Year (Sp. 2, Age 1)'= meanspe2a1, 'Min Number Escaped Per Year (Sp. 2, Age 1)'= minspe2a1, 'Max Number Escaped Per Year (Sp. 2, Age 1)'= maxspe2a1,
                              'Mean Number Escaped Per Year (Sp. 2, Age 2)'= meanspe2a2,  'Min Number Escaped Per Year (Sp. 2, Age 2)'= minspe2a2, 'Max Number Escaped Per Year (Sp. 2, Age 2)'= maxspe2a2,
                              'Mean Number Escaped Per Year (Sp. 2, Age 3)'= meanspe2a3,  'Min Number Escaped Per Year (Sp. 2, Age 3)'= minspe2a3, 'Max Number Escaped Per Year (Sp. 2, Age 3)'= maxspe2a3,
                              'Mean Number Escaped Per Year (Sp. 2, Age 4)'= meanspe2a4,  'Min Number Escaped Per Year (Sp. 2, Age 4)'= minspe2a4, 'Max Number Escaped Per Year (Sp. 2, Age 4)'= maxspe2a4 )
    both2 <- both2 %>% mutate(across(1:12, round, 2))
     datatable(both2, options = list(dom = 't') )

  })
  
  
  
  output$PlotTwoSpecies<- renderPlot({
    maint <- final2()
    repair <- repair2()
    upfront <- upfront()
    ef <- ef()
    esc <- escapement()
    esc2 <- escapement2()
    twospec <-cbind(esc, esc2)
    #cost1 <- cost3 %>% mutate(total4 = (total1+total2+total3), meanperyearesc = (meanperyearesc1+meanperyearesc2+meanperyearesc3)) %>% select(total4, meanperyearesc) %>% rename(total1 = total4)
    #cost1
    
    two <- twospec %>% mutate(totalcomb = ((total1+total2+total3+total4)+totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4)) %>% select(totalcomb)
    twoef <- cbind(ef, two)
    
    CostEf <- twoef %>% mutate(CostEff = (totalcomb * (1-ef))) %>% select(CostEff)
    
    
    var <- cbind(maint, repair, upfront, CostEf)
    comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront+CostEff), scenario='Barrier') %>% select(total, scenario)
    
    esc <- escapement()
    esc2 <- escapement2()
    twospec <-cbind(esc, esc2)
    two <- twospec %>% mutate(totalcomb = ((total1+total2+total3+total4)+(totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4)))
    # %>% rename('Total for Species 1 - age 1' = 'total1', 'Total for Species 1 - age 2' = 'total2', 'Total for Species 1 - age 3' = 'total3', 'Total for Species 2' = 'totalsp2')
    esca <- two %>% mutate(scenario='No Barrier') %>% select(totalcomb, scenario)
    
    as.data.frame(comb)
    as.data.frame(esca)
    
    comb$total = as.numeric(comb$total)
    esca$total = as.numeric(esca$totalcomb)
    cmbine <- rbind(comb, esca)
    
    ggplot(cmbine, aes(x=total, color=scenario, fill=scenario))+
      geom_density(alpha=.5)+theme_classic()+xlab('Predicted distribution of cost')+ylab('Density')+scale_fill_manual(values=c('black', 'grey85'))+scale_color_manual(values=c('black', 'grey85'))+
      theme(axis.title=element_text(color="black", size='16'), axis.ticks = element_line(color="black"),axis.text=element_text(color="black", size='16'))+
      theme(legend.position = "top")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      scale_x_continuous(labels = scales::comma)+scale_y_continuous(labels = scales::comma)
  })
  
  
  output$introtext <- renderText({
    HTML(paste0("<b>", "Costs and benefits of barriers to prevent reservoir fish escapement", "</b>"))
    
  })
  
  output$plotfinalsp1<- renderPlot({
    maint <- final2()
    repair <- repair2()
    upfront <- upfront()
    
    var <- cbind(maint, repair, upfront)
    
    ef <- ef()
    esc <- escapement()
    
    twoef <- cbind(ef, esc)
    
    CostEf <- twoef %>% mutate(CostEff = ((total1+total2+total3+total4)* (1-ef))) %>% select(CostEff)
    
    
    
    
    var <- cbind(maint, repair, upfront, CostEf)
    comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront+CostEff), scenario='Barrier') %>% select(total, scenario)
    
    esc <- escapement()
    
    esca <- esc %>% mutate(scenario='No Barrier') %>% select(total1,total2,total3, total4, scenario) %>% mutate(totale = (total1+total2+total3+total4))
    
    as.data.frame(comb)
    as.data.frame(esca)
    
    comb$total = as.numeric(comb$total)
    esca$total = as.numeric(esca$totale)
    cmbine <- rbind(comb, esca)
    
   
    
    ggplot(cmbine, aes(x=total, color=scenario, fill=scenario))+
      geom_density(alpha=.5)+theme_classic()+xlab('Predicted distribution of cost')+ylab('Density')+scale_fill_manual(values=c('black', 'grey85'))+scale_color_manual(values=c('black', 'grey85'))+
      theme(axis.title=element_text(color="black", size='16'), axis.ticks = element_line(color="black"),axis.text=element_text(color="black", size='16'))+
      theme(legend.position = "top")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      scale_x_continuous(labels = scales::comma)+scale_y_continuous(labels = scales::comma)
  })
  
  
  output$plotfinalsp2<- renderPlot({
    maint <- final2()
    repair <- repair2()
    upfront <- upfront()
    
    
    ef <- ef()
   
    esc2 <- escapement2()
    
    
    twoef <- cbind(ef, esc2)
    CostEf <- twoef %>% mutate(CostEff = (totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4)* (1-ef)) %>% select(CostEff)
    var <- cbind(maint, repair, upfront, CostEf)
    
    
    
    comb <- var %>% group_by(SimNum) %>% mutate(total = (repsumPV + mainsumPV+upfront+CostEff), Scenario='Barrier') %>% select(total, Scenario)
    
    one<- escapement2()
    
    
    esca <- one %>% mutate(Scenario='No barrier') %>% mutate(totalsp2 = totalsp2a1+totalsp2a2+totalsp2a3+totalsp2a4) %>% select(totalsp2, Scenario)
    
    
    as.data.frame(comb)
    as.data.frame(esca)
    
    comb$total = as.numeric(comb$total)
    esca$total = as.numeric(esca$totalsp2)
    cmbine <- rbind(comb, esca)
    
    ggplot(cmbine, aes(x=total, color=Scenario, fill=Scenario))+
      geom_density(alpha=.5)+theme_classic()+xlab('Predicted distribution of cost')+ylab('Density')+scale_fill_manual(values=c('black', 'grey85'))+scale_color_manual(values=c('black', 'grey85'))+
      theme(axis.title=element_text(color="black", size='16'), axis.ticks = element_line(color="black"),axis.text=element_text(color="black", size='16'))+
      theme(legend.position = "top")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      scale_x_continuous(labels = scales::comma)+scale_y_continuous(labels = scales::comma)
  })
  
  output$sp1label <- renderText({ paste("COST OF BARRIER VS. ESCAPEMENT OF SPECIES 1 (", input$Species1, ")")})
  
  output$sp2label <- renderText({ paste("COST OF BARRIER VS. ESCAPEMENT OF SPECIES 2 (", input$Species2, ")")})
  
  
  output$EscCostText <- renderText({
    
    'To quantify the cost of escapement, which represents the value of fish lost due to escapement, we used replacement cost metrics. Replacement cost accounts for the cost of stocking a fish, and the survival from stocking to the age of interest (in this case, age at escapement), to determine how much it will cost to produce one fish that reaches the age of interest (Southwick and Loftus 2016). <br> <br> 
          
          Survival is an additive rate; therefore, survival in the application needs to be converted into an instantaneous rate first, and this rate is then transformed back to a discrete rate which represents the total number of fish that survive from stocking to age at escapement (more details in diagram below). Currently, the app allows for a different survival rate post-stocking, and a constant adult survival rate. The interactive nature of the application enables users to visualize how different survival rates effect the end results, which can be vital in situations where there is uncertainty regarding what the actual rates are.<br> <br>  
          
          One important note about the application is that it currently quantifies escapement cost as a direct replacement value. This metric has advantages: it is objective, transparent and reproducible,. However, replacement costs metrics likely reflect a conservative “value” of fish, because they do not take into account the use value of the fish (for example, how much is that fish worth to an angler). For low density species such as Muskellunge, this use value is likely high per fish; therefore, other metrics of fish value should be considered beyond direct replacement costs.'
    

  })
  
 output$equation3 <- renderText({ ' 
    Equation 3: equation for calculating the present value of a terminating annual series cash flow, where a = annual cost, i = discount rate (2% real rate), n = planning horizon (number of years)' 
 })


 output$equation1 <- renderText({ '
    Equation 1: equation for converting adult survival rate (A) to instantaneous survival rate (Z)' 
 })
 
 output$equation2 <- renderText({ ' 
    Equation 2: equation for transforming instantaneous rate back to discrete survival rate from post stocking to age at escapement' 
 })

  output$HowTo <- renderText({
    'The application is set to default values which represent the costs to construct a barrier on Brushy Creek Lake in Iowa in 2020. On the sidebar, you can select and drag values to evaluate how changes in spillway size, 
      or various construction costs metrics will change the cost distribution over time. <br> <br>
For fish escapement cost metrics, these values are based on Muskellunge (species 1) and Walleye (species 2) stocking and survival metrics in Brushy Creek Lake, IA. As the sidebar elements are moved, the app will automatically 
    update plots and tables. The first plot below shows the cost distribution for two species and the barrier, and results for the species individually are located below this plot. <br> <br>
The Barrier Cost Table and Escapement Cost Table tabs store and display results form individual simulations. <br> <br>
    For more details on how the model works and the inputs, please navigate to the Model Mechanics tab.'
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("BarrierCost", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(testvec(), file, row.names = FALSE)
    }
  )
  

  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("EscapementCostSpecies1Only", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(escsp1(), file, row.names = FALSE)
    }
  )
  
  
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste("EscapementCostSpecies2Only", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(escsp2(), file, row.names = FALSE)
    }
  )
  
  
  output$downloadData_HowTo <- downloadHandler(
    filename = "BarrierApplicationUserGuide.pdf",
    content = function(file) {
      file.copy("www/HowToFishBarrierApp.pdf", file)
    }
  )

  output$downloadData_Meta <- downloadHandler(
    filename = "BarrierApplicationMetadata.xlsx",
    content = function(file) {
      file.copy("www/BarrierApplicationMetadata.xlsx", file)
    }
  )

  
  
  
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("EscapementCost", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(esc(), file, row.names = FALSE)
    }
  )
  
  
  

  
  
}


shinyApp(ui = ui, server = server)
