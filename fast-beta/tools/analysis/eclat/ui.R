#source('tools/analysis/eclat/eclattest.R')

# untuk memindahkan panel kembali ke data setiap berpindah menu
observe({
  if(!identical(input$nav_fast, "Principal Component Analysis")){
    updateTabsetPanel(session, "pcatab", selected = "pcanalysis")
  }
})

upload <- reactive({
  getdata()
})

output$eclat <- renderUI({
  sidebarLayout(
    sidebarPanel(
      div(class="busy", p("Calculation in progress..."), img(src="ajaxloaderq.gif")),
      uiOutput(outputId = 'nav_menu'),
      uiOutput(outputId = 'input_x'),
      uiOutput(outputId = 'input_y')
    ),
    mainPanel(
      div(class="busy", p("Calculation in progress..."), img(src="ajaxloaderq.gif")),
      tabsetPanel(
        id = 'al_tav',
        tabPanel(
          title = "tabel",
          uiOutput('tampil_tabel')
        ),
        tabPanel(
          title = "plot",
          plotOutput('tampil_plot')
        )
      )
      
    )
  )
})

output$input_x <- renderUI({
  vars<- upload()
  selectInput(inputId= "x", label= "Pilih X: ",
              choices= names(vars), selected= state_multvar("x", vars), multiple=F, selectize= F)  
})

output$input_y <- renderUI({
  vars<- upload()
  selectInput(inputId= "y", label= "Pilih y: ",
              choices= names(vars), selected= state_multvar("y", vars), multiple=F, selectize= F)  
})

output$tampil_tabel <- renderTable({
  datah <- upload()
  x <- datah[,input$x]
  y <- datah[,input$y]
  
  cbind(x,y)
})

output$tampil_plot <- renderPlot({
  datah <- upload()
  x <- datah[,input$x]
  y <- datah[,input$y]
  
  plot(x,y)
})

output$nav_menu <- renderUI({
  wellPanel(
    HTML(paste("<label><strong>Menu:","Association Rule","</strong></label>")),
    HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
    HTML(paste("<label><strong>Data:",input$datasets,"</strong></label>"))
  ) # tool status
})