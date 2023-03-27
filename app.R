if (!requireNamespace("shiny","shinyFiles","shinydashboard",
                      "iterators","parallel","foreach","doParallel",
                      "regress","Matrix","lme4","EMMREML","dplyr","erer",
                      "car","rsconnect","xts","zoo","performanceAnalytics",
                      "data.table","ggpolt2","BGLR")) 
  install.packages("shiny","shinyFiles","shinydashboard",
                   "iterators","parallel","foreach","doParallel",
                   "regress","Matrix","lme4","EMMREML","dplyr","erer",
                   "car","rsconnect","xts","zoo","performanceAnalytics",
                   "data.table","ggpolt2","BGLR")

library(shiny)
library(shinyFiles)
library(shinydashboard)
library(iterators)
library(parallel)
library(foreach)
library(doParallel)
library(regress)
library(Matrix)
library(lme4)
library(EMMREML)
library(dplyr)
library(erer)
library(car)
library(rsconnect)
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(data.table)
library(ggplot2)
library(BGLR)

ui <- dashboardPage(
  dashboardHeader(title = "SIMPLEGEBV"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Phenotype Correction",tabName = "phe_correc",icon = icon("th")),
      menuItem("GEBV(Cross-Validation)",tabName = "GBLUP_EMMREML",icon = icon("th")),
      menuItem("GEBV(no CV)",tabName = "GEBV_no",icon = icon("list-alt")),
      menuItem("GEBV_Bayes",tabName = "GEBV_BGLR",icon = icon("list-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "phe_correc",
              box(title = "Phenotype correction",width = 2,solidHeader = T,status = "primary",collapsible = TRUE,
                  fileInput("pheno_pc", "Choose phen File",
                            multiple = FALSE),
                  tags$hr(),
                  checkboxInput("header_pc", "Header", FALSE),
                  radioButtons("sep_pc", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t",
                                           Space = " "),
                               selected = ","),
                  radioButtons("quote_pc", "Quote",
                               choices = c(None = "",
                                           "Double Quote" = '"',
                                           "Single Quote" = "'"),
                               selected = ""),
                  tags$hr(),
                  radioButtons("disp_pc", "Display",
                               choices = c(Head = "head",
                                           All = "all"),
                               selected = "head")
                  
              ),
              
              box(title = "Parameter_set",width = 2,solidHeader = T,status = "primary",collapsible = TRUE,
                  column(12,
                         numericInput("Fixed", 
                                      h3("Number of Fixed Effects"), 
                                      value = 1,min = 1),
                  ),
                  
                  column(12,
                         numericInput("Covar", 
                                      h3("Number of Covariance"), 
                                      value = 0,min = 0),
                         br(),
                         br(),
                  ),
                  
                  
                  
                  column(12,
                         actionButton("run_pc", "Execute",width = 147),
                         br(),
                         downloadButton("downloadPic_pc", "Download Picture"),
                         br(),
                         downloadButton("downloadResult_pc", "Download Results "),
                         br(),
                         downloadButton("downloadANOVA_pc", "Download ANOVA"),
                         br(),
                         br(),
                         
                  )
                  
              ),
              
              tabBox(
                title = "", width = 8,
                id = "tabset_pc1",height = "300px",
                tabPanel("Original_Phe",tableOutput("pheno_pcout")),
                tabPanel("Correction_Phe",verbatimTextOutput("pheno_correctout")),
              ),
              
             
              
              tabBox(
                title = "", width = 8,
                id = "tabset_pc2",height = "600px",
                tabPanel("Plot",plotOutput("out_plot_pc")),
                tabPanel("ANOVA",verbatimTextOutput("out_ANOVA_pc")),
              )
        
        
      ),
      
      tabItem(tabName = "GBLUP_EMMREML",
              box(title = "File Input",width = 9,solidHeader = T,status = "primary",collapsible = TRUE,
                  column(4,
                         fileInput("phen", "Choose phen File",
                                   multiple = FALSE),
                         tags$hr(),
                         checkboxInput("header1", "Header", FALSE),
                         radioButtons("sep1", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t",
                                                  Space = " "),
                                      selected = " "),
                         radioButtons("quote1", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = ""),
                         tags$hr(),
                         radioButtons("disp1", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")),
                  column(4,
                         fileInput("fam", "Choose fam File",
                                   multiple = FALSE),
                         tags$hr(),
                         checkboxInput("header2", "Header", FALSE),
                         radioButtons("sep2", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t",
                                                  Space = " "),
                                      selected = " "),
                         radioButtons("quote2", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = ""),
                         tags$hr(),
                         radioButtons("disp2", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")),
                  column(4,
                         fileInput("grm", "Choose grm File",
                                   multiple = FALSE),
                         tags$hr(),
                         checkboxInput("header3", "Header", FALSE),
                         radioButtons("sep3", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t",
                                                  Space = " "),
                                      selected = " "),
                         radioButtons("quote3", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = ""),
                         tags$hr(),
                         radioButtons("disp3", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")),
              ),
              
              box(title = "Cross-Validation Set",width = 3,solidHeader = T,status = "primary",collapsible = TRUE,
                  br(),
                  numericInput("rep", 
                               h3("Times of Repetition"), 
                               value = 10,min = 1,max = 50),
                  br(),
                  tags$hr(),
                  numericInput("Fold", 
                               h3("Fold"), 
                               value = 5,min = 1,max = 50),
                  br()
              ),
              
              tabBox(
                title = "", width = 4,
                id = "tabset1",height = "300px",
                tabPanel("File phen",tableOutput("phen1")),
                tabPanel("File fam",tableOutput("fam1")),
                tabPanel("File grm",tableOutput("grm1")),
              ), 
              
              tabBox(
                title = "", width = 5,
                id = "tabset2",height = "300px",
                tabPanel("Mean_correlation",verbatimTextOutput("Mean_correlation1")),
                tabPanel("Mean_regression",verbatimTextOutput("Mean_regression1")),
                tabPanel("GEBV(part)",verbatimTextOutput("ID_GEBV"))
              ), 
              
              box(title = "Function Button",width = 3,solidHeader = T,status = "primary",
                  actionButton("match", "Execute",width = 132),
                  br(),
                  tags$hr(),
                  downloadButton("downloadData1", "Download M_c"),
                  br(),
                  tags$hr(),
                  downloadButton("downloadData2", "Download M_r"),
                  br(),
                  tags$hr(),
                  downloadButton("downloadData3", "Download GEBV")
              )
      ),
      
      tabItem(tabName = "GEBV_no",
              box(title = "File Input",width = 6,solidHeader = T,status = "primary",collapsible = TRUE,
                  column(4,
                         fileInput("pheno", "Choose phen File",
                                   multiple = FALSE),
                         tags$hr(),
                         checkboxInput("header11", "Header", FALSE),
                         radioButtons("sep11", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t",
                                                  Space = " "),
                                      selected = " "),
                         radioButtons("quote11", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = ""),
                         tags$hr(),
                         radioButtons("disp11", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")),
                  column(4,
                         fileInput("file_fam", "Choose fam File",
                                   multiple = FALSE),
                         tags$hr(),
                         checkboxInput("header22", "Header", FALSE),
                         radioButtons("sep22", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t",
                                                  Space = " "),
                                      selected = " "),
                         radioButtons("quote22", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = ""),
                         tags$hr(),
                         radioButtons("disp22", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")),
                  column(4,
                         fileInput("file_grm", "Choose grm File",
                                   multiple = FALSE),
                         tags$hr(),
                         checkboxInput("header33", "Header", FALSE),
                         radioButtons("sep33", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t",
                                                  Space = " "),
                                      selected = " "),
                         radioButtons("quote33", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = ""),
                         tags$hr(),
                         radioButtons("disp33", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")),
              ),
              box(title = "Options",width = 6,solidHeader = T,status = "primary",collapsible = TRUE,
                  column(6,
                         actionButton("calculate", "Execute",width = 135),
                  br()
                  ),
                  
                  column(6,
                  downloadButton("downloadData4", "Download GEBV"),
                  br()
                  )
              ),
              
              tabBox(
                title = "", width = 6,
                id = "tabset1",height = "300px",
                tabPanel("File phen",tableOutput("pheno1")),
                tabPanel("File fam",tableOutput("file_fam1")),
                tabPanel("File grm",tableOutput("file_grm1")),
                tabPanel("GEBV(part)",verbatimTextOutput("file_gebv")),
              ), 
      ),
      tabItem(tabName = "GEBV_BGLR",
              box(title = "File Input",width = 4,solidHeader = T,status = "primary",collapsible = TRUE,
                  column(6,
                         fileInput("pheno_bayes", "Choose phen File",
                                   multiple = FALSE),
                         tags$hr(),
                         checkboxInput("header_bayes", "Header", FALSE),
                         radioButtons("sep_bayes", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t",
                                                  Space = " "),
                                      selected = " "),
                         radioButtons("quote_bayes", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = ""),
                         tags$hr(),
                         radioButtons("disp_bayes", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")),
                  column(6,
                         fileInput("SNP_bayes", "Choose chip data",
                                   multiple = FALSE),
                         tags$hr(),
                         checkboxInput("header_bayes_SNP", "Header", FALSE),
                         radioButtons("sep_bayes_SNP", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t",
                                                  Space = " "),
                                      selected = " "),
                         radioButtons("quote_bayes_SNP", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = ""),
                         tags$hr(),
                         radioButtons("disp_bayes_SNP", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")),
              ),
              
              box(title = "Parameters",width = 8,solidHeader = T,status = "primary",collapsible = TRUE,
                  column(3,
                         numericInput("nIter", 
                                      h4("Number of Iteration"), 
                                      value = 1000,min = 10),
                         
                  ),
                  
                  column(3,
                         numericInput("burnIn", 
                                      h4("Number of Burn-in"), 
                                      value = 100, min =1),
                         
                  ),
                  
                  column(3,
                         numericInput("thin", 
                                      h4("Thin"), 
                                      value = 5,min = 1),
                         
                  ),
                  
                  column(3,
                         numericInput("ProbIn", 
                                      h4("ProbIn(for BayesB and C)"), 
                                      value = 0.05)
                  ),
              ),
              
              box(title = "Options",width = 8,solidHeader = T,status = "primary",collapsible = TRUE,
                  
                  column(3,
                         numericInput("randomNum", 
                                      h4("The number of this program"), 
                                      value = 2048)
                  ),
                  
                  column(3,
                         selectInput("model_set", label = h4("Select model"), 
                                     choices = list("BayesA" = "BayesA", "BayesB" = "BayesB", "BayesC" = "BayesC"), 
                                     selected = 2),
                  ),
                  
                  column(2,
                         br(),
                         br(),
                         actionButton("run_BGLR", "Execute",width = 130),
                         
                  ),
                  
                  column(2,
                         br(),
                         br(),
                         downloadButton("downloadTrain", "Training Population"),
                         
                         
                  ),
                  column(2,
                         br(),
                         br(),
                         downloadButton("downloadValid", "Validation Population"),
                  )
              ),
              
              tabBox(
                title = "", width = 8,
                id = "tabset_bglr",height = "550px",
                tabPanel("File Phenotype",tableOutput("pheno_bayes_out")),
                tabPanel("File Chip Data",tableOutput("SNP_bayes_SNP_out")),
                tabPanel("Distribution",plotOutput("distribution_plot")),
              ), 
      )
    )
  )
)


server <- function(input, output,session) {
  options(shiny.maxRequestSize=10000*1024^2)
  
  output$pheno_pcout <- renderTable({
    req(input$pheno_pc)
    tryCatch(
      {
        df <- read.table(input$pheno_pc$datapath,
                         header = input$header_pc,
                         sep = input$sep_pc ,
                         quote = input$quote_pc)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$disp_pc == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  observeEvent(input$run_pc,{
    
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, 
                 expr = {
                   aa<-read.table(input$pheno_pc$datapath,
                                  header = input$header_pc,
                                  sep = input$sep_pc,
                                  quote = input$quote_pc)
                   k = input$Fixed 
                   z = input$Covar 
                   d = k+z+2
                   nam <- c(paste0("factor",c(1:k)))
                   colnum <- as.numeric(ncol(aa))
                   model2 <- nam[1]
                   
                   ANOVAdata<-list()
                   combinedata <- list()
                   for (i in 1:k) {
                     colnames(aa)[i+2] <- nam[i]
                     assign(nam[i],aa[,i+2])
                   }
                   
                   if(k > 1){
                     for (i in 2:k) {
                       model2 <- paste(model2,nam[i],sep = "+")
                     }
                   }else{
                     model2 <- model2
                   }
                   
                   if(z != 0){
                     fam <- c(paste0("cov",c(1:z)))
                     for (i in 1:z) {
                       colnames(aa)[i+k+2] <- fam[i]
                       assign(fam[i],aa[,i+k+2])
                       model2 <- paste(model2,fam[i],sep = "+")
                     }
                     last_model <- paste0("y~",model2)
                   }else{
                     last_model <- paste0("y~",model2)
                   }
                   
                   pic <- as.data.frame(aa[,2])
                   colnames(pic)[1]="IID"
                   
                   for (e in (d+1):(colnum)) {
                     #去除NA值
                     na.row <- is.na(aa[,e])
                     no_na <- aa[complete.cases(aa[,e]),]
                     mean_data <- mean(no_na[,e])
                     sd_data <- sd(no_na[,e])
                     minum <- mean_data-3*sd_data
                     maxum <- mean_data+3*sd_data
                     
                     remove_data <- no_na[which(no_na[,e]<minum | no_na[,e]> maxum),c(1:d,e)]
                     remain_data <- no_na[which(no_na[,e]>minum & no_na[,e]< maxum),c(1:d,e)]
                     
                     y  <-  remain_data[,ncol(remain_data)]
                     colnames(remain_data)[ncol(remain_data)] = "y"
                     
                     #计算效应值及显著性检验（F检验）
                     lhw=lm(last_model,data=remain_data)
                     Anova(lhw,data=remain_data, type = 3)
                     #工作路径
                     AAA <- Anova(lhw,data=remain_data, type = 3)
                     residual <- lhw$residuals
                     phe_qc <- cbind(remain_data[,1:2],residual)
                     colnames(phe_qc)[3] <- colnames(aa)[e]
                     
                     pic_phe_qc <- phe_qc[,2:3]
                     colnames(pic_phe_qc)[1] <- "IID"
                     pic=dplyr::left_join(pic,pic_phe_qc,by="IID")
                     
                     combinedata[[e-d]] <- phe_qc
                     ANOVAdata[[e-d]] <- AAA
                     
                   }
                   incProgress(1/2)
                   pic_t <- as.data.frame(pic[,-1])
                   
                   outfile_name <- tempfile(fileext = '.png')
                   png(outfile_name,bg="white",width = 4000,height = 4000,res = 400)
                   if(ncol(pic_t) > 1){
                     show_pic<-chart.Correlation(pic_t, histogram=TRUE, pch=19)
                   }else{
                     colnames(pic_t)[1] = colnames(aa)[colnum]
                     show_pic <- hist(pic_t[,1], xlab = colnames(pic_t)[1], ylab = 'Value', main=paste0('Distribution of the ',colnames(pic_t)[1]), col = 'brown',breaks = 50)
                   }
                   dev.off()
                   
                   
                   incProgress(2/2)
                   Sys.sleep(0.25)
                   
                   output$pheno_correctout <- renderPrint(head(pic))
                   output$out_plot_pc <- renderPlot({
                     if(ncol(pic_t) > 1){
                       chart.Correlation(pic_t, histogram=TRUE, pch=19)
                     }else{
                       colnames(pic_t)[1] = colnames(aa)[colnum]
                       hist(pic_t[,1], xlab = colnames(pic_t)[1], ylab = 'Value', main=paste0('Distribution of the ',colnames(pic_t)[1]), col = 'brown',breaks = 50)
                     }
                   })
                   
                   
                   
                   
                   
                   output$out_ANOVA_pc <- renderPrint(head(ANOVAdata))
                   
                   output$downloadPic_pc <- downloadHandler(
                     filename = function(){
                       paste('Pic_', Sys.Date(), '.PNG', sep='')
                     },
                     content = function(file){
                       file.copy(outfile_name, file)
                     })
                   
                   output$downloadResult_pc <- downloadHandler(
                     filename = function(){
                       paste('Correc_result_', Sys.Date(), '.csv', sep='')
                     },
                     content = function(file){
                       write.csv(pic, file)
                     })
                   
                   output$downloadANOVA_pc <- downloadHandler(
                     filename = function(){
                       paste('ANOVA_', Sys.Date(), '.csv', sep='')
                     },
                     content = function(file){
                       write.csv(ANOVAdata, file)
                     })
                   
                   
                   
                 })
    
  })
  
  
  output$phen_1 <- renderTable({
    req(input$file_csv)
    tryCatch(
      {
        df <- read.table(input$file_csv$datapath,
                         header = input$header_1,
                         sep = input$sep_1 ,
                         quote = input$quote_1)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$disp_1 == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  
  output$phen1 <- renderTable({
    req(input$phen)
    tryCatch(
      {
        df <- read.table(input$phen$datapath,
                         header = input$header1,
                         sep = input$sep1 ,
                         quote = input$quote1)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$disp1 == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$fam1 <- renderTable({
    req(input$fam)
    tryCatch(
      {
        df <- read.table(input$fam$datapath,
                         header = input$header2,
                         sep = input$sep2 ,
                         quote = input$quote2)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$disp2 == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$grm1 <- renderTable({
    req(input$grm)
    tryCatch(
      {
        df <- read.table(input$grm$datapath,
                         header = input$header3,
                         sep = input$sep3 ,
                         quote = input$quote3)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$disp3 == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  observeEvent(input$match,{
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0,
                 expr = {
                   
                   predictMartini <- function(Y,V,training,topredict){  
                     n <- sum(training)  
                     total <- nrow(V)
                     yvalid <- Y[topredict]  
                     GG <- V/(max(V))  
                     Pheno <- Y[training]  
                     abc <- emmreml(y=Y[training], X=cbind(matrix(rep(1, n), ncol=1)),Z=diag(total)[training,],  
                                    K=GG)  
                     abc$sigma <- c(GG = abc$Vu,In = abc$Ve)
                     
                     pred1 <- abc$uhat  
                     yvalidBC <- pred1[topredict]  
                     cor1 <- cor(yvalid,as.vector(yvalidBC))  
                     bb1 <- lm(yvalid ~ as.vector(yvalidBC))$coefficients[2]  
                     return(list(cor = cor1,bb = bb1,vc = abc$sigma,gebv=pred1))  
                   }  
                   
                   y<-read.table(input$phen$datapath,
                                 header = input$header1,
                                 sep = input$sep1,
                                 quote = input$quote1)
                   G<-read.table(input$grm$datapath,
                                 header = input$header2,
                                 sep = input$sep2,
                                 quote = input$quote2)
                   id<-read.table(input$fam$datapath,
                                  header = input$header3,
                                  sep = input$sep3,
                                  quote = input$quote3)
                   
                   
                   GG <- as.matrix(G)
                   rownames(G)<-id[,2]
                   colnames(G)<-id[,2]
                   colnames(id)[2]="ID"
                   colnames(y)[2]="ID"
                   c=dplyr::left_join(id,y,by="ID")
                   y=c[,c(1:2,8)]
                   y0<-y[,3]
                   
                   acc<-c();bb<-c();gebv<-c()
                   acc_list <- list()  
                   bb_list <- list()   
                   gebv_list<-list()
                   ID_GEBV <- list()
                   group <- matrix(0,length(y0),input$rep)
                   
                   LOG.GBLUP <- T
                   
                   for (i in 1:input$rep){
                     set.seed(i^2+12)
                     group[,i] <- sample(1:input$Fold,length(y0),replace=T,prob=rep(1/input$Fold,input$Fold))
                   }
                   
                   for(i in 1:(input$rep*input$Fold)){
                     
                     if(i %% input$Fold == 0){kk <- i/input$Fold;jj <- input$Fold}else{kk <- as.integer(i/input$Fold) + 1;jj <- i %% input$Fold}
                     groupkk <- group[,kk]
                     tgroup <- groupkk == jj
                     refergroup <- groupkk != jj
                     
                     if (LOG.GBLUP){
                       res <- predictMartini(y0,GG,refergroup,tgroup)
                       gebv<-res$yvalidBC
                       acc2 <- res$cor; unbias2 <- res$bb;gebv2<-res$gebv
                       acc <- c(acc2); bb <- c(unbias2);gebv<-c(gebv2)
                     }
                     acc_list[[i]] <- c(kk,jj,acc);bb_list[[i]] <- c(kk,jj,bb);gebv_list[[i]]<-c(kk,jj,gebv)
                     incProgress(1/(input$rep*input$Fold))
                     Sys.sleep(0.25)
                     
                   }
                   
                   RES <- Reduce("rbind",acc_list);colnames(RES) <- c("rep","fold","GHBLUP")
                   BB <- Reduce("rbind",bb_list);colnames(BB) <- c("rep","fold","GHBLUP")
                   GEBV<-Reduce("rbind",gebv_list);colnames(BB) <- c("rep","fold","GEBV")
                   GEBV2<-GEBV[,-c(1:2)]
                   GEBV3<-as.data.frame(colMeans(GEBV2))
                   GEBV4<-data.frame(y,GEBV3)
                   GEBV5<-GEBV4[,-1]
                   GEBV6<-GEBV5[,-2]
                   colnames(GEBV6)<-c("ID","GEBV")
                   
                   
                   ACCUR<-c()
                   bbia<-c()
                   MEAN_ACC<-mean(RES[,3])
                   MEAN_COR<-mean(BB[,3])
                   ACCUR<-rbind(ACCUR,MEAN_ACC)
                   bbia<-rbind(bbia,MEAN_COR)
                   output$Mean_correlation1 <- renderPrint(head(ACCUR))
                   output$Mean_regression1 <- renderPrint(head(bbia))
                   output$ID_GEBV <- renderPrint(head(GEBV6))
                   
                   output$downloadData1 <- downloadHandler(
                     filename = function(){
                       paste('Mean_Coeffcient-', Sys.Date(), '.csv', sep='')
                     },
                     content = function(file){
                       write.csv(ACCUR, file)
                     })
                   
                   output$downloadData2 <- downloadHandler(
                     filename = function(){
                       paste('Mean_Regression-', Sys.Date(), '.csv', sep='')
                     },
                     content = function(file){
                       write.csv(bbia, file)
                     })
                   
                   output$downloadData3 <- downloadHandler(
                     filename = function(){
                       paste('GEBV-', Sys.Date(), '.csv', sep='')
                     },
                     content = function(file){
                       write.csv(GEBV6, file)
                     })
                 })
  })
  
  output$pheno1 <- renderTable({
    req(input$pheno)
    tryCatch(
      {
        df <- read.table(input$pheno$datapath,
                         header = input$header11,
                         sep = input$sep11 ,
                         quote = input$quote11)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$disp11 == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$file_fam1 <- renderTable({
    req(input$file_fam)
    tryCatch(
      {
        df <- read.table(input$file_fam$datapath,
                         header = input$header22,
                         sep = input$sep22 ,
                         quote = input$quote22)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$disp22 == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$file_grm1 <- renderTable({
    req(input$file_grm)
    tryCatch(
      {
        df <- read.table(input$file_grm$datapath,
                         header = input$header33,
                         sep = input$sep33 ,
                         quote = input$quote33)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$disp33 == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  observeEvent(input$calculate,{
    
    
    predictgebv <- function(Y,V,training,topredict){  
      n <- sum(training)  
      total <- nrow(V) 
      yvalid <- Y[topredict]  
      GG <- V/(max(V))  
      Pheno <- Y[training]  
      abc <- emmreml(y=Y[training], X=cbind(matrix(rep(1, n), ncol=1)),Z=diag(total)[training,],  
                     K=GG)  
      abc$sigma <- c(GG = abc$Vu,In = abc$Ve)  
      print(abc$sigma)
      
      pred1 <- abc$uhat  
      yvalidBC <- pred1[topredict]  
      return(list(gebv=pred1))
    }  
    
    y<-read.table(input$pheno$datapath,
                  header = input$header11,
                  sep = input$sep11,
                  quote = input$quote11)
    G<-read.table(input$file_grm$datapath,
                  header = input$header22,
                  sep = input$sep22,
                  quote = input$quote22)
    id<-read.table(input$file_fam$datapath,
                   header = input$header33,
                   sep = input$sep33,
                   quote = input$quote33)
    
    GG <- as.matrix(G)
    colnames(id)[2]="ID"
    colnames(y)[2]="ID"
    c=dplyr::left_join(id,y,by="ID")#以ID为准匹配y中的值并返回至id文件中，同时保持id文件的每一行,？双冒号指定包中某功能
    y=c[,c(1:2,8)]#选取c的1，2，8列，及每行(列可以使用（1，2，8）也可以（1：2，8）)
    y0<-y[,3]#选取y的第三列
    rownames(G)<-id[,2]
    colnames(G)<-id[,2]
    
    tgroup <- is.na(y0)
    refergroup <- !tgroup
    res <- predictgebv(y0,GG,refergroup,tgroup)
    gebv<-res$yvalidBC
    gebv2<-res$gebv
    GEBV <- cbind(y[,2],gebv2)
    colnames(GEBV)<-c("ID","GEBV")
    
    output$file_gebv <- renderPrint(head(GEBV))
    
    output$downloadData4 <- downloadHandler(
      filename = function(){
        paste('gebv', Sys.Date(), '.csv', sep='')
      },
      content = function(file){
        write.csv(GEBV, file)
      })
  })
  
  output$pheno_bayes_out <- renderTable({
    req(input$pheno_bayes)
    tryCatch(
      {
        df <- read.table(input$pheno_bayes$datapath,
                         header = input$header_bayes,
                         sep = input$sep_bayes ,
                         quote = input$quote_bayes)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$disp_bayes == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$SNP_bayes_SNP_out <- renderTable({
    req(input$SNP_bayes)
    tryCatch(
      {
        df <- fread(input$SNP_bayes$datapath,
                    header = input$header_bayes_SNP,
                    sep = input$sep_bayes_SNP ,
                    quote = input$quote_bayes_SNP)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    if(input$disp_bayes_SNP == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  observeEvent(input$run_BGLR,{
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0,
                 expr = {
    
    pheno_info<-read.table(input$pheno_bayes$datapath,
                           header = input$header_bayes,
                           sep = input$sep_bayes,
                           quote = input$quote_bayes)
    geno_info<-fread(input$SNP_bayes$datapath,
                     header = input$header_bayes_SNP,
                     sep = input$sep_bayes_SNP,
                     quote = input$quote_bayes_SNP)
    #geno_info<-fread(input$SNP_bayes$datapath,header = T,fill = TRUE)
    
    
    nIter=input$nIter  ### number of iteration 设置迭代次数
    burnIn=input$burnIn  ### burnin a part of iteration 设置去掉的迭代次数，也就是说这里迭代2000次，去掉500次，用筛选到的1500次作为计算结果
    thin = input$thin
    probIn = input$ProbIn
    K = input$randomNum
    incProgress(1/9)
    Sys.sleep(0.25)
    
    if(is.null(K)){
      x_seed <- sample(89765:123467846,1)
      K = x_seed
    }else{
      K = K
    }
    set.seed(as.numeric(K))  ### 设置随机种子
    geno <- as.matrix(geno_info[,-1])  ### 只保留基因型数据
    
    Bayes_NA <- sum(is.na(pheno_info$V2))                             #表型文件存在NA，将NA作为验证群
    if(Bayes_NA != 0){
      valid <- is.na(pheno_info$V2)
      train <- !valid
      pheno <- as.matrix(pheno_info$V2)
    }else{
      group <- sample(1:5,nrow(pheno_info),replace=T,prob=rep(1/5,5)) #表型文件不存在NA，则随机选取20%作为验证群体，余下80%作为参考群体
      valid  <-  group == 1
      train= group != 1
      pheno_info$V2[valid] <- NA 
      pheno <- as.matrix(pheno_info$V2)
    }
    
    incProgress(2/9)
    Sys.sleep(0.25)
    
    if(input$model_set == "BayesA"){
      myBGLR=BGLR(y=pheno, 
                  ETA=list(MRK =list(X=geno ,model='BayesA',saveEffects=FALSE)),
                  nIter=nIter,
                  burnIn=burnIn,
                  thin = thin)  ### 模型预测
      
      pred=predict(myBGLR)      ### 预测结果
      pred.valid=pred[valid]    ### 验证群体的预测结果
      pred.train=pred[train]    ### 参考群体的预测结果
      pred.valid.ID=pheno_info[,1][valid]
      pred.train.ID=pheno_info[,1][train]
      result_valid <- cbind(pred.valid.ID,pred.valid)
      result_train <- cbind(pred.train.ID,pred.train)
    }else{
      myBGLR=BGLR(y=pheno, 
                  ETA=list(MRK =list(X=geno ,model=input$model_set,probIn=probIn,saveEffects=FALSE)),
                  nIter=nIter,
                  burnIn=burnIn,
                  thin = thin)  ### 模型预测
      
      pred=predict(myBGLR)      ### 预测结果
      pred.valid=pred[valid]    ### 验证群体的预测结果
      pred.train=pred[train]    ### 参考群体的预测结果
      pred.valid.ID=pheno_info[,1][valid]
      pred.train.ID=pheno_info[,1][train]
      result_valid <- cbind(pred.valid.ID,pred.valid)
      result_train <- cbind(pred.train.ID,pred.train)
    }
    
    incProgress(7/9)
    Sys.sleep(4)
    
    temp <- as.data.frame(cbind(pheno_info$V2[complete.cases(pheno_info$V2)],myBGLR$yHat[train]))
    colnames(temp)[1] <- "y" ; colnames(temp)[2] <- "yHat"
    output$distribution_plot <- renderPlot(
      ggplot(temp, aes(x=yHat, y=y)) +
        ggtitle("Distribution")+
        geom_point(
          color="orange",
          fill="#67c9ff",
          shape=21,
          alpha=0.5,
          size=4,
          stroke =1.5
        )
    )
    
    incProgress(8/9)
    Sys.sleep(2)
    incProgress(9/9)
    
    output$downloadTrain <- downloadHandler(
      filename = function(){
        paste('Training_GEBV', Sys.Date(), '.csv', sep='')
      },
      content = function(file){
        write.csv(result_train, file)
      })
    output$downloadValid <- downloadHandler(
      filename = function(){
        paste('Validation_GEBV', Sys.Date(), '.csv', sep='')
      },
      content = function(file){
        write.csv(result_valid, file)
      })
   })
  })

}

shinyApp(ui = ui, server = server)