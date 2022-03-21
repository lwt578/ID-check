library(shiny)
library(tidyverse)

checkID <- function(id){
  weight <- data.frame(x=1:17,
                       y=c(7,9,10,5,8,4,2,1,6,3,7,9,10,5,8,4,2))
  check <- data.frame(Z=0:10,
                      M=c(1,0,"X",9,8,7,6,5,4,3,2))
  
  M1 <- str_sub(id,-1,-1)
  id17 <- str_sub(id,1,17)
  if(str_length(id17)<17){
    return(FALSE)
  }else{
    num <- c()
    for (i in 1:17){
      num[i]=str_sub(id17,i,i) %>% 
        as.integer()
      if(is.na(num[i])){
        return(FALSE)
      }
    }
    
    Z1 <- sum(num*weight$y) %% 11
    
    for (i in 1:11){
      if (Z1==check$Z[i]){
        if(M1==check$M[i]){
          return(TRUE)
        }else{
          return(FALSE)
        }
      }
    }
    
  }
  
}

ui <- fluidPage(

  titlePanel("校验身份证号码"),
  
  fluidRow(
    column(1,),
    column(10,
      tabsetPanel(
        id = "methods",
        type = "tabs",
        tabPanel("单个身份证号",
                 textInput("id","输入身份证号:"),
                 actionButton("run", "点击验证"),
                 actionButton("cls", "重置"),
                 textOutput("message")
        ),
        tabPanel("批量输入", 
                 textAreaInput("idm","批量输入身份证号:"),
                 actionButton("runm", "点击验证"),
                 actionButton("clsm", "重置"),
                 textOutput("messagem")
        ),
        tabPanel("导入文件",
                 fileInput("upload","导入文件……",accept = c(".csv",".xls",".xlsx")),
                 actionButton("runupload", "点击验证"),
                 textOutput("messageupload")
        )
      )     
    ),
    column(1,)
  )
)


server <- function(input, output, session) {
  observeEvent(input$select, {
    updateTabsetPanel(inputId = "methods",selected = input$select)
  }) 
  
  observeEvent(input$cls, {
    updateTextInput(session,"id",value = "")
    output$message <- renderText({  })
  }) 
  
  observeEvent(input$clsm, {
    updateTextAreaInput(session,"idm",value = "")
    output$messagem <- renderText({ })
  }) 
  
  load_file <- function(name, path) {
    f <- strsplit(name,"[.]")[[1]][2]
    switch (f,
            csv = readr::read_csv(path),
            xls = readxl::read_xls(path),
            xlsx = openxlsx::read.xlsx(path),
            validate("Invalid file; Please upload a excel or csv file")
    )
  }
    
  df <- reactive({
    req(input$upload)
    load_file(input$upload$name,input$upload$datapath)
    
  })
      
  observeEvent(input$run, {
    req(input$id)
    r <- checkID(input$id)
    
    if(r){
      result <- '校验通过'
    }else{
      result <- '身份证号码错误'
    }
    
    output$message <- renderText({
      result
    })
    
  })
  
  observeEvent(input$runm, {
    req(input$idm)
    m <- str_split(input$idm,"\n")
    r <- map_lgl(m,checkID)
    
    if(mean(r)==1){
      result <-'全部校验通过'
    }else{
      n <- which(r==FALSE)
      result <-paste(c('身份证号码错误，行号(不含标题):',n))
    }
    
    output$messagem <- renderText({
      result
    })
    
  })
  
  observeEvent(input$runupload, {
    req(df())
    r <- map_lgl(df()$身份证号,checkID)
    
    if(mean(r)==1){
      result <-'全部校验通过'
    }else{
      n <- which(r==FALSE)
      result <-paste(c('身份证号码错误，行号:',n))
    }
    
    output$messageupload <- renderText({
      result
    })
    
  })

}

 
shinyApp(ui, server)
