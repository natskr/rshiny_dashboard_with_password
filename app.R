# read in necessary libraries
library(RPostgreSQL)
library(openssl)
library(digest)
library(shiny)
library(shinydashboard)
library(data.table)

# # Encrypted password file set up (only run when you set passwords)
# pwd_file <- read.csv(text="username,password
#                            username,password",
#                      header=TRUE, 
#                      stringsAsFactors=FALSE)
# pwd_file$password <- sha512(pwd_file$password)

# read in table with encrypted password
tbl = read.csv(text=rawToChar(pwd_file), 
	       file="/../pwd_file.csv", 
	       stringsAsFactors=FALSE)

# log into DB
password <- scan(file = '/../pwd_file_DB.csv', what=character())
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, 
                 host=host, 
                 port=port,
                 dbname=dbname, 
                 user=user, 
                 password=password)

# Define UI for the dashboard (including sidebar and body)
ui<- dashboardPage(
  dashboardHeader(title = "Dashboard Title", titleWidth=350),
  # define sidebar width and menu items
  dashboardSidebar(width = 350, 
                   sidebarMenu(id="tabs",
                               menuItem("Wait for the Login Window", 
					tabName="start_page", 
					icon=icon("user-circle")),        
                               menuItem("Sub-Menu", 
					tabName="sub-menu",
					icon=icon("user-circle")))),
  # define body of dashboard and a box with a download button
  dashboardBody(
    tabItems(
      tabItem(tabName="sub-menu",
              fluidRow(box(title="Box Title", 
                           width=5, status="primary", solidHeader=TRUE, 
			   collapsible=TRUE, responsive=TRUE,
                           downloadButton("download_file","Download Button Text")))))))

# Define server file for the dashboard including password verification
server <- shinyServer(function(input, output, session) {
  values <- reactiveValues(authenticated=FALSE)

  # password verification pop-up
  dataModal <- function(failed=FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer=tagList(actionButton("ok", "OK")))}
  
  obs1 <- observe({showModal(dataModal())})
  
  # password verification logic including encryption
  obs2 <- observe({
    req(input$ok)
    isolate({Username <- input$username
             Password <- sha512(input$password)})
    Id.username <- which(tbl[,1] == Username)
    Id.password <- which(tbl[,2] == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
          Logged <<- TRUE
          values$authenticated <- TRUE
          obs1$suspend()
          removeModal()
      } else {values$authenticated <- FALSE}}})
  

  # download button logic
  data_table_to_download <- as.data.table(dbGetQuery(con, "SELECT * FROM table;"))
  
  output$download_file<-downloadHandler(
    filename = function(){
      paste("FileName_",Sys.Date(),".csv",sep="")},
      content=function(file){
      write.csv(data_table_to_download$column,file,row.names = FALSE)})
  })

# Run the application 
shinyApp(ui = ui, server = server)

# dbDisconnect(dbListConnections(drv)[[1]])