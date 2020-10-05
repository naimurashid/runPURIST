#' @export
server <- function(input, output){
  
  
  volumes = shinyFiles::getVolumes()()
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
    
    # pass metric
    genetsp = mat[rownames(mat) %in% classifs[[1]]$TSPs,]
    sub = apply_classifier(data = genetsp,classifier = classifs[[1]])
    negcontrol = mat[grep("NEG",rownames(mat)),]
    poscontrol = mat[grep("POS",rownames(mat)),]
    Metric = round(colMeans(log(genetsp+1))-colMeans(log(negcontrol+1)), 3)
    pass = c("FAIL","PASS")[(Metric > 3)^2 + 1]
    
    
    
    out = apply_classifier(data = mat, classifier = classifs[[1]])
    output.path = sprintf("%s/results_of_purist_by_nanostring.csv", datapath)
    #print(output.path)
    
    # not outputting pass yet
    out = cbind(rownames(out), out, Metric)
    colnames(out)[1] = "Specimen ID"
    suppressWarnings(write.csv(out, file = output.path, row.names = F,col.names = T, quote = F))
    #
    #for(i in 1:nrow(out)){
    #  # create patient directory
    #  name = rownames(out)[i]
    #  subdir = sprintf("%s/%s/", datapath,name)
    #  dir.create(subdir)
      
      # write patient specific report
    #  output.sub.path = sprintf("%s/%s_report.csv", subdir, name)
    #  dd = matrix(c(rownames(out)[i],out[i,]), nrow = 1, ncol= ncol(out)+1)
    #  colnames(dd) = c("",colnames(out))
    #  write.csv(dd, file = output.sub.path, row.names = F, col.names = T, quote = F)
      
      # create heatmap
    #  jpeg(sprintf("%s/%s_report_heatmap.jpeg", subdir, name))
    #  pheatmap(mat[,i], cluster_cols = F)
    #  dev.off()
    #}
    #
    
    # create heatmap
    #jpeg(sprintf("%s/results_of_purist_by_nanostring_expression_heatmap.jpeg",dir))
    #  mate = mat[match(as.vector(t(classifs[[1]]$TSPs)),rownames(mat)),]
    #  mate = mate[!is.na(mate[,1]),]
    #  try(pheatmap(log(mate+1), cluster_cols = F))
    #  if(is.character(try)) print("could not print heatmap")
    #dev.off()
    
    # look at total expr of 16 genes, and then house keepers separately
    
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






