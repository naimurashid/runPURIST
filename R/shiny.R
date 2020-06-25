#' @export
server <- function(input, output){
  
  
  volumes = shinyFiles::getVolumes()
  shinyDirChoose(
    input,
    'dir',
    roots = volumes,
    filetypes = c('RCC', 'rcc')
  )
  
  
  
  
  myData <- reactive({
    
    if(is.null(input$dir)) return(NULL)
    inFile <- input$dir
    datapath <- parseDirPath(volumes, input$dir)
    
    #data <- read.csv(inFile$datapath, header = TRUE)
    #data
    rcc = read_rcc(datapath)
    dat = rcc$raw
    dat
  })
  
  myResult <- reactive({
    inFile <- input$dir
    if (is.null(inFile)) return(NULL)
    
    datapath <- parseDirPath(volumes, input$dir)
    
    #data <- read.csv(inFile$datapath, header = TRUE)
    #data
    rcc = read_rcc(datapath)
    dat = rcc$raw
    mat = data.matrix(dat[,-c(1:3)])
    rownames(mat) = as.matrix(dat[,2])
    
    out = apply_classifier(data = mat, classifier = classifs[[1]])
    output.path = sprintf("%s/report.csv", datapath)
    print(output.path)
    write.csv(out, file = output.path, row.names = T, col.names = T, quote = F)
    out
  })
  
  
  output$contents <- DT::renderDataTable({
    DT::datatable(myData())       
  })
  
  output$result <- DT::renderDataTable({
    DT::datatable(myResult())       
  })
  
  
  
}

#' @export
ui<- shinyUI(fluidPage(
  titlePanel("PurIST Point and Click"),
  sidebarLayout(
    sidebarPanel(
      #  fileInput('file1', 'Choose CSV File',
      #            accept=c('text/csv',
      #                     'text/comma-separated-values,text/plain',
      #                     '.csv'))
      #),
      shinyDirButton("dir", "Choose directory", "Upload")
      , width = 2.5),
    mainPanel(
      div(DT::dataTableOutput("contents"), style = "font-size: 75%"),
      br(),
      br(),
      div(DT::dataTableOutput("result"), style = "font-size: 75%"),
      width = 9.5,
      fluid = F
    )
  )
)
)

#' @export
run <- function(){
  shinyApp(ui,server)
}






