library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(shinydashboard)
library(shinythemes)
library(readxl)
library(openxlsx)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(DBI)
library(pool)
library(uuid)


########################Create sql database#############################

pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite")

#Create df for data entry into sql
employees_df <- data.frame(   title=character(),
                              first_name=character(),
                              last_name=character(),
                              job_title=character(),
                              room=character(),
                              email=character(),
                              phone_internal=character(),
                              department=character(),
                              picture=character(),
                              monday=character(),
                              tuesday=character(), 
                              wednesday=character(),
                              thursday=character(),
                              friday=character(),
                              saturday=character(),
                              sunday=character(),
                              note=character(),
                              date=character(),
                              image_ext=character(),
                              row_id=character(),
                              stringsAsFactors = FALSE)

##load permanent df to store all submitted samples
##Only has to be performed once

#dbWriteTable(pool, "employees_df", employees_df, temporary = FALSE, overwrite = FALSE)
#data <- dbReadTable(pool, "employees_df")

##################Load data in sql database from Excel#################

#add a unique row id
#unique_id <- function(data){
#    replicate(nrow(data), UUIDgenerate())
#}

#employees_data <- read_excel("Dir_Loc")
#employees_data$date <- format(Sys.Date())
#employees_data$row_id <- unique_id(employees_data)
#quary <- sqlAppendTable(pool, "employees_df", employees_data, row.names = FALSE)
#dbExecute(pool, quary)

###################################Button functions####################

imageDownloadbutton <- function(outputId, label = NULL){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("images"), label)
}

excelDownloadbutton <- function(outputId, label = NULL){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("file-excel"), label)
}

tableDownloadbutton <- function(outputId, label = NULL){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("download"), label)
}

####################UI################################################

ui <- fluidPage(title = "Employee Directory",
    theme = shinytheme("darkly"),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "submit-form.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "profile-form.css")
    ),
    #Supress all error messages. Final app only.
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    useShinyjs(),
    shinyalert::useShinyalert(),
    navbarPage(title = div(class="navbar-title", "Employee Directory")),
    div(class="button-container", align="right",
        circleButton("add_button", icon = icon("plus"), status = "success",
                     size = "lg")),
    fluidRow(
    div(class="search-container",
        div(class="search-input", 
            textInput("search_field", label = NULL, value = "", width = NULL, placeholder = "Search")),
        uiOutput("table_filters"))),
    uiOutput("download"),
    dataTableOutput("employees_table"),
        div(class="footer-container",
            uiOutput("footer_date"),
            div(class="footer-title", HTML("Noveld &copy; - Shiny Contest 2020"))),
    tags$script(src="index.js"))
   
#################Server############################################

server <- function(input, output, session) {
  
##################Data############################################
  
employees_df <- reactive({
    
  #make reactive to
  input$submit
  input$yes_button
  input$edit_button
  input$edit_button2

  employees_df <- dbReadTable(pool, "employees_df")
  return(employees_df)
})
 
#Filtered DF
 
employees_df_filtered <- reactive({
  
  #make reactive to
  input$submit
  input$yes_button
  input$edit_button
  input$edit_button2

  #needed to filter table again for reactivity
  employees_filtered <- dbReadTable(pool, "employees_df")
  
  if(!input$department_filter == "all"){employees_filtered <- employees_filtered %>% filter(grepl(input$department_filter, department, ignore.case = TRUE))}
  
  if(!input$room_filter == "all"){employees_filtered <- employees_filtered %>% filter(grepl(input$room_filter, room, ignore.case = TRUE))}
  
  return(employees_filtered)
  
})

employees_df_table <- reactive({
  
  #make reactive to
  input$submit
  input$yes_button
  input$edit_button2
  input$edit_button
  
  employees_filtered <- dbReadTable(pool, "employees_df")
  
  if(!input$department_filter == "all"){employees_filtered <- employees_filtered %>% filter(grepl(input$department_filter, department, ignore.case = TRUE))}
  
  if(!input$room_filter == "all"){employees_filtered <- employees_filtered %>% filter(grepl(input$room_filter, room, ignore.case = TRUE))}
  
  
  employees_df_table <- employees_filtered %>% select(picture, image_ext, title, first_name, last_name, job_title, room, phone_internal, email, date, row_id)
  
  employees_df_table$employee <- paste0( "<b>", employees_df_table$title, " ",
                                         employees_df_table$first_name, " ",
                                         employees_df_table$last_name, "</b><br>",
                                         '<div class="table-sub-title">',employees_df_table$job_title, "</div>")
  employees_df_table$email <- str_replace(employees_df_table$email, employees_df_table$email, sprintf('<a href="mailto:%s">%s</a>',  employees_df_table$email, employees_df_table$email))
  employees_df_table$view <- paste("<button id=\"info_button\" 
                                              type=\"button\" 
                                              class=\"btn btn-primary btn-sm\"
                                              onclick=\"Shiny.onInputChange(&quot;info_button&quot;,  Math.random())\"><i class=\"fa fa-address-card fa-2x\"></i></button>")
  employees_df_table$actions <-paste("<button id=\"edit_button\" 
                                              type=\"button\" 
                                              class=\"btn btn-link btn-sm\"
                                              onclick=\"Shiny.onInputChange(&quot;edit_button&quot;,  Math.random())\"><i class=\"fa fa-edit fa-2x\"></i></button>",
                                     "<button id=\"delete_button\" 
                                              type=\"button\" 
                                              class=\"btn btn-link btn-sm\"
                                              onclick=\"Shiny.onInputChange(&quot;delete_button&quot;,  Math.random())\"><i class=\"fa fa-times fa-2x\"></i></button>") 
  
  #Replace picture with anonymous picture when No
  employees_df_table$picture[employees_df_table$picture == "No"] <- paste0('<img class="profile-table-img" src="no_picture.png"></img>')
  
  #Replace picture with profile picture when Yes
  employees_df_table$picture <- str_replace(employees_df_table$picture, "Yes", sprintf('<img class="profile-table-img" src="profile_images/%s_%s_%s_%s.%s"></img>', 
                                                                                       paste(employees_df_table$date), 
                                                                                       paste(employees_df_table$first_name), 
                                                                                       paste(employees_df_table$last_name),
                                                                                       paste(employees_df_table$row_id),
                                                                                       paste(employees_df_table$image_ext)
  ))
  employees_df_table <- employees_df_table %>% select(picture, employee, view, room, phone_internal, email, actions, row_id)
  return(employees_df_table)
})


download_df <- reactive({
  
  download_df <- dbReadTable(pool, "employees_df")
  download_df <- download_df %>% select(last_name, first_name, title, job_title, room, phone_internal, department)
  
  return(download_df)
  
})

###################Data Filters###########################

output$table_filters <- renderUI({
  
div(class="filter-container",
      dropdownButton(
        selectInput("department_filter", label= "Department", multiple = FALSE, choices = c("all", unique(sort(tolower(employees_df()$department))))),
        selectInput("room_filter", label= "Room", multiple = FALSE, choices = c("all", unique(sort(tolower(employees_df()$room))))),
        circle = TRUE, status = "info", 
        icon=icon("filter"), width ="200px", size = "sm",
        tooltip = tooltipOptions(title ="Click to see filters.")))
  
})

observeEvent(input$department_filter, {
  

updateSelectInput(session, "room_filter", label= "Room", selected = input$room_filter,  choices = c("all", unique(tolower(sort(employees_df_filtered()$room[])))))
  
})

observeEvent(input$room_filter, {
  
updateSelectInput(session, "department_filter", label= "Department", selected = input$department_filter,  choices = c("all", unique(tolower(sort(employees_df_filtered()$department[])))))

})


#####Download data buttons################################################

output$download_button <- downloadHandler(
  filename = function() {"employee_directory.xlsx"},
  content = function(file){write.xlsx(download_df(), file)
  
})

output$excel_download <- downloadHandler(
  filename = function() {"employee_directory_full.xlsx"},
  content = function(file){write.xlsx(employees_df(), file)
    
  })

output$image_download <- downloadHandler(
  filename <- function() {
    paste("profile_images", "tar", sep=".")
  },
  
  content <- function(file) {
    tar(file, "www/profile_images/")
  }
)

#################################################Login#################################################################


observeEvent(input$login_link,{

  showModal(
    modalDialog(div(class="login-modal", div(class="login-header", h4("Enter Login Details"))),
                textInput("userInp", "Login"),
                passwordInput("pwInp", "Password"),
                p(id="errorMessage", html("&nbsp;")),
                actionButton("butLogin", "login", class = 'btn action-button btn-success', icon = icon('sign-in')),
                div(class="login-footer", actionButton("dismiss", "Dismiss")),
                size = "s",
                easyClose = TRUE,
                footer = NULL))
})


user <- reactiveValues(login = FALSE, name = NULL, header = NULL)

observeEvent(input$butLogin,{
  
  req(input$userInp, input$pwInp)
  if(input$userInp != "admin"){
    user$login <- FALSE
    user$header <- "Incorrect information."
    html("errorMessage", paste0(user$header))
  } else {
    if (input$pwInp == "SHINY2020") {  ## match
      user$login <- TRUE
      user$name <- input$userInp
      removeModal()
    } else {  ## no match
      user$login <- FALSE
      user$header <- 'Incorrect information.'
      html("errorMessage", paste0(user$header))
    }
  }
})

output$download <- renderUI({
  
  if(user$login == FALSE){
  div(class="download-container",
      tableDownloadbutton("download_button", label=NULL),
      bsTooltip(id = "download_button", title = "Download table as Excel file.", 
                placement = "left", trigger = "hover"),
      actionLink("login_link", "Login"))} 
  else{
    div(class="download-container",
        imageDownloadbutton("image_download", label=NULL),
        bsTooltip(id = "image_download", title = "Download images as .tar file.", 
                  placement = "left", trigger = "hover"),
        excelDownloadbutton("excel_download", label=NULL),
        bsTooltip(id = "excel_download", title = "Download backup of database as Excel file.", 
                  placement = "left", trigger = "hover"),
        downloadButton("download_button", label=NULL, icon = icon("download")),
        bsTooltip(id = "download_button", title = "Download table as Excel file.", 
                  placement = "left", trigger = "hover"),
        actionLink("login_link", "Login"))} 
      
})

observeEvent(input$dismiss,{
  removeModal()
  
})

#################################################Profile Modal################################

observeEvent(input$info_button, {
  
  sel_row <- input$employees_table_row_last_clicked
  row_id <- employees_df_filtered()[sel_row, "row_id"]
  table <- dbReadTable(pool, "employees_df")
  
  #Profile values
  
  title <- table[table$row_id == row_id, "title"]
  profile_date <- table[table$row_id == row_id, "date"]
  image_ext <- table[table$row_id == row_id, "image_ext"]
  first_name <- table[table$row_id == row_id, "first_name"]
  last_name <- table[table$row_id == row_id, "last_name"]
  
  picture <- table[table$row_id == row_id, "picture"]
  profile_image <- if(picture == "No"){HTML('<img class="profile-img" src="no_picture.png" height="120"></img>')} else{ 
  profile_image <- if(picture == "Yes"){HTML(sprintf('<img class="profile-img" src="profile_images/%s_%s_%s_%s.%s" height="120"></img>', 
                                                           paste(profile_date),  
                                                           paste(first_name), 
                                                           paste(last_name),
                                                           paste(row_id),
                                                           paste(image_ext)))}}

  job_title <- table[table$row_id == row_id, "job_title"]
  room <- table[table$row_id == row_id, "room"]
  email <- table[table$row_id == row_id, "email"]
  phone_internal <- table[table$row_id == row_id, "phone_internal"]
  department <- table[table$row_id == row_id, "department"]

  monday <- table[table$row_id == row_id, "monday"]
  tuesday <- table[table$row_id == row_id, "tuesday"]
  wednesday <- table[table$row_id == row_id, "wednesday"]
  thursday <- table[table$row_id == row_id, "thursday"]
  friday <- table[table$row_id == row_id, "friday"]
  saturday <- table[table$row_id == row_id, "saturday"]
  sunday <- table[table$row_id == row_id, "sunday"]
  
  if(nchar(monday) == 0){monday <- "Not Defined"}
  if(nchar(tuesday) == 0){tuesday <- "Not Defined"}
  if(nchar(wednesday) == 0){wednesday <- "Not Defined"}
  if(nchar(thursday) == 0){thursday <- "Not Defined"}
  if(nchar(friday) == 0){friday <- "Not Defined"}

  note <- table[table$row_id == row_id, "note"]

  showModal(
    modalDialog(id="profile_form",
      title = NULL,
      footer = NULL,
      easyClose = TRUE,
      div(
          fluidPage(
            htmlTemplate(
              filename = "www/profile_form.html",
              profile_image = profile_image,
              title = title,
              first_name = paste(" ", first_name, " "),
              last_name = last_name,
              job_title = job_title,
              department = department,
              room = room,
              phone_internal = phone_internal,
              email = tags$a(href=sprintf("mailto:%s", email), email),
              monday = monday,
              tuesday = tuesday,
              wednesday = wednesday,
              thursday = thursday,
              friday = friday,
              saturday = saturday,
              sunday = sunday,
              note = note
              
              )))))

})

#################################################Add data################

##########Functions


#List of mandatory fields for submission
fieldsMandatory <- c("title", "first_name", "last_name", "job_title", "room", "email", "phone_internal", "department")

#define which input fields are mandatory 
observe({
    
    #Make reactive to add button
    input$submit
    input$edit_button2
    
    mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                   !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    shinyjs::toggleState(id = "edit_button2", condition = mandatoryFilled)
})

#define which input fields should be saved and recorded at time of submission
fieldsAll <- c("title", "first_name", "last_name", "job_title", "room", "email", "phone_internal",
               "department", "monday", "tuesday", 
               "wednesday", "thursday", "friday", "saturday", "sunday", "note")  

#add a unique row id
unique_id <- function(data){
    replicate(nrow(data), UUIDgenerate())
    #paste0(seq.int(nrow(data)), round(runif(nrow(data),10000000,99999999), 0)) 
}

#Needed to reset fileInput
cachedFile <- reactiveValues(
  datapath = NULL
)

observeEvent(input$add_button, {
  
  File <- input$picture_upload 
  cachedFile$datapath <- File$datapath
  entry_form()
  
})

observeEvent(input$edit_button, priority = 20,{
  
  File <- input$picture_upload 
  cachedFile$datapath <- File$datapath
  
})

#Remove any malicious html code
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

#save form data into data_frame format
formData <- reactive({
    
    #check if file has already been submitted
    inputFile <- input$picture_upload
    file_cached <- identical(cachedFile$datapath, inputFile$datapath)
    
    data <- sapply(fieldsAll, function(x) (cleanFun(input[[x]])))
    data <- t(data)
    data <- data.frame(data)
    data <- c(data, 
              date=format(Sys.time(), "%Y%m%d_%H%M%S"), 
              row_id = unique_id(data))
    if(file_cached == FALSE){
    data <- c(data, picture = paste("Yes"), image_ext = tools::file_ext(inputFile$name))
    } else{
    data <- c(data, picture = paste("No"), image_ext = paste("ND"))
    }
    
    return(data)
}) 

#Append data to sql table
appendData <- function(data){
    quary <- sqlAppendTable(pool, "employees_df", data, row.names = FALSE)
    dbExecute(pool, quary)
}

form <- function(title, button_id, button_name){
    
    #add otherwise modal is only triggered once
    input$add_button
    input$edit_button

    showModal(
        modalDialog(id = "entry_form",
            title = title,
            div(
                fluidPage(
                    htmlTemplate(
                        filename = "www/entry_form.html",
                        title = selectInput("title", label= NULL, 
                                                           selected = NULL, 
                                                           multiple = FALSE,
                                                           choices = c("", "Ms.", "Mr.")),
                        first_name =  textInput("first_name", label=NULL, placeholder = NULL, width = "100%"),
                        last_name = textInput("last_name", label=NULL, placeholder = NULL, width = "100%"),
                        job_title = textInput("job_title", label=NULL, placeholder = NULL, width = "100%"),
                        room = textInput("room", label=NULL, placeholder = NULL, width = "100%"),
                        email = textInput("email", label=NULL, placeholder = NULL, width = "100%"),
                        phone_internal = textInput("phone_internal", label=NULL, placeholder = NULL, width = "100%"), 
                        department = textInput("department", label=NULL, placeholder = NULL, width = "100%"),
                        picture_upload = fileInput("picture_upload", label = NULL, multiple = FALSE, width = "100%", accept = c('image/png', 'image/jpeg')), 
                        monday_input = selectInput("monday", label= NULL, multiple = FALSE, choices = c("Please select"='',"Full Day", "Morning", "Afternoon", "Not working", "Home office")),
                        tuesday_input = selectInput("tuesday", label= NULL, multiple = FALSE, choices = c("Please select"='',"Full Day", "Morning", "Afternoon", "Not working", "Home office")),
                        wednesday_input = selectInput("wednesday", label= NULL, multiple = FALSE, choices = c("Please select"='',"Full Day", "Morning", "Afternoon", "Not working", "Home office")),
                        thursday_input = selectInput("thursday", label= NULL, multiple = FALSE, choices = c("Please select"='',"Full Day", "Morning", "Afternoon", "Not working", "Home office")),
                        friday_input = selectInput("friday", label= NULL, multiple = FALSE, choices = c("Please select"='',"Full Day", "Morning", "Afternoon", "Not working", "Home office")),
                        saturday_input = selectInput("saturday", label= NULL, multiple = FALSE, choices = c("Not working", "Full Day", "Morning", "Afternoon", "Home office")),
                        sunday_input = selectInput("sunday", label= NULL, multiple = FALSE, choices = c("Not working" ,"Full Day", "Morning", "Afternoon", "Home office")),
                        note_input = textInput("note", label=NULL, placeholder = "e.g. Monday morning home office.", width = "100%"),
                        submit_button = actionButton(button_id, button_name, icon("save"))),
                        easyClose =TRUE))))
    
    
}

###Add profile picture
saveData <- function(data, outputDir, rowid = formData()$row_id) {
  
  # Create a unique file name
  fileName <- sprintf("%s_%s_%s_%s.%s",  
                      paste(formData()$date),
                      paste(formData()$first_name),
                      paste(formData()$last_name),
                      paste(rowid),
                      paste(formData()$image_ext))
  # Write the file to the local system
  file.copy(data$datapath, file.path(outputDir, fileName), overwrite = TRUE
  )
} 

entry_form <- reactive({
    
    form("New Entry", "submit", "Save")
    
})

succes_alert <- reactive({
    
    #make reactive to
    input$submit
    
    shinyalert("Thank you!", "Your data has been submitted.", type = "success", 
               confirmButtonCol = "#337ab7")
    
})

###Observers

observeEvent(input$submit, priority = 20, {
    
  file_ext <- c("JPG", "jpg", "PNG", "png")
  File <- input$picture_upload
  
  #check if first name or last name containes illegal characters
  #check if first name or last name containes illegal characters
  names <- c(input$first_name, input$last_name)
  names <- gsub(" ", "", names, fixed = TRUE)
  name_check <- grepl("^[a-zA-Z0-9_-]*$", names)
  illegal_char <- any(name_check == FALSE)
  
  file_cached <- identical(cachedFile$datapath, File$datapath)
  
  if(is.null(File)){File$name <- "photo.jpg"}
  #Check for correct file format
  if (!tools::file_ext(File$name) %in% file_ext){
    shinyalert("Error", "Wrong file format. Only .jpg and .png allowed.", type = "error", 
               confirmButtonCol = "#337ab7")
  } else if(illegal_char == TRUE){
    shinyalert("Error", "First Name or Last Name contains illegal characters.", type = "error", 
               confirmButtonCol = "#337ab7")
  } else{
    
    appendData(formData())
    if(file_cached == FALSE){saveData(data = input$picture_upload, outputDir = "www/profile_images")}
    shinyjs::reset("entry_form")
    removeModal()
    succes_alert()
  }
})

#################################Edit################################################

edit_form <- reactive({
  
  #Make reactive to add button

  form("Edit Entry", "edit_button2", "Save")
  
})

observeEvent(input$edit_button, priority = 20, {
  
  sel_row<- isolate(input$employees_table_row_last_clicked) 
  row_id <- employees_df_table()[sel_row, "row_id"] 
  cachedRowid$row_id <- row_id
})

observeEvent(input$edit_button,{
  
    sel_row <- input$employees_table_row_last_clicked
    row_id <- employees_df_filtered()[sel_row, "row_id"] 
    table <- employees_df_filtered()

    edit_form()
    
    updateSelectInput(session, "title", selected = table[table$row_id == row_id, "title"])
    updateTextInput(session, "first_name", value = table[table$row_id == row_id, "first_name"])
    updateTextInput(session, "last_name", value = table[table$row_id == row_id, "last_name"])
    updateTextInput(session, "job_title", value = table[table$row_id == row_id, "job_title"])
    updateTextInput(session, "room", value = table[table$row_id == row_id, "room"])
    updateTextInput(session, "email", value = table[table$row_id == row_id, "email"])
    updateTextInput(session, "phone_internal", value = table[table$row_id == row_id, "phone_internal"])
    updateTextInput(session, "department", value = table[table$row_id == row_id, "department"])
    updateSelectInput(session, "monday", selected = table[table$row_id == row_id, "monday"])
    updateSelectInput(session, "tuesday", selected = table[table$row_id == row_id, "tuesday"])
    updateSelectInput(session, "wednesday", selected = table[table$row_id == row_id, "wednesday"])
    updateSelectInput(session, "thursday", selected = table[table$row_id == row_id, "thursday"])
    updateSelectInput(session, "friday", selected = table[table$row_id == row_id, "friday"])
    updateSelectInput(session, "saturday", selected = table[table$row_id == row_id, "saturday"])
    updateSelectInput(session, "sunday", selected = table[table$row_id == row_id, "sunday"])
    updateTextInput(session, "note", value = table[table$row_id == row_id, "note"])
    
})

removeData <- function(rowid = formData()$row_id) {
  
  # Create a unique file name
  delfile <- dir(path="www/profile_images", pattern=paste(rowid))
  file.remove(file.path("www/profile_images", delfile))
} 

renameData <- function(rowid = formData()$row_id){
  
  table <- employees_df_filtered()
  
  
  date <- table[table$row_id == rowid, "date"]
  image_ext <- table[table$row_id == rowid, "image_ext"]
  
  
  fileName <- sprintf("%s_%s_%s_%s.%s",  
                      date,
                      input$first_name,
                      input$last_name,
                      rowid,
                      image_ext)
  
  
  renfile <- dir(path="www/profile_images/", pattern=paste(rowid))
  file.rename(paste0("www/profile_images/", renfile), paste0("www/profile_images/", fileName))
}

observeEvent(input$edit_button2, {
  
  File <- input$picture_upload
  row_id <-  cachedRowid$row_id 
  
  #check if file has already been submitted
  inputFile <- input$picture_upload
  file_cached <- identical(cachedFile$datapath, File$datapath)
  
  #check if first name or last name containes illegal characters
  names <- c(input$first_name, input$last_name)
  names <- gsub(" ", "", names, fixed = TRUE)
  name_check <- grepl("^[a-zA-Z0-9_-]*$", names)
  illegal_char <- any(name_check == FALSE)

if(illegal_char == TRUE){
  shinyalert("Error", "First Name or Last Name contains illegal characters.", type = "error", 
             confirmButtonCol = "#337ab7")
} else if(file_cached == FALSE){
    
    file_ext <- c("JPG", "jpg", "PNG", "png")

    #Check for correct file format
    if (!tools::file_ext(File$name) %in% file_ext){
      shinyalert("Error", "Wrong file format. Only .jpg and .png allowed.", type = "error", 
                 confirmButtonCol = "#337ab7")}    

    image_ext <- tools::file_ext(File$name)
    profile_date <- format(Sys.time(), "%Y%m%d_%H%M%S")

    removeData(rowid = row_id)
    
    dbExecute(pool, sprintf('UPDATE "employees_df" SET "date" = ?, "title" = ?, "first_name" = ?, "last_name" = ?, 
                          "job_title" = ?, "room" = ?, "email" = ?, "phone_internal" = ?,
                          "department" = ?, "picture" = ?, "image_ext" = ?, "monday" = ?, "tuesday" = ?,
                          "wednesday" = ?, "thursday" = ?, "friday" = ?, "saturday" = ?, "sunday" = ?, "note" = ?
                          WHERE "row_id" = ("%s")', row_id), 
              param = list(profile_date,
                           input$title,
                           cleanFun(input$first_name),
                           cleanFun(input$last_name),
                           cleanFun(input$job_title),
                           cleanFun(input$room),
                           cleanFun(input$email),
                           cleanFun(input$phone_internal),
                           cleanFun(input$department),
                           "Yes",
                           image_ext,
                           input$monday,
                           input$tuesday,
                           input$wednesday,
                           input$thursday,
                           input$friday,
                           input$saturday,
                           input$sunday,
                           input$note
              ))
    saveData(data = input$picture_upload, outputDir = "www/profile_images", rowid = row_id)
    removeModal() 
} else{
  
  dbExecute(pool, sprintf('UPDATE "employees_df" SET "title" = ?, "first_name" = ?, "last_name" = ?, 
                          "job_title" = ?, "room" = ?, "email" = ?, "phone_internal" = ?,
                          "department" = ?, "monday" = ?, "tuesday" = ?,
                          "wednesday" = ?, "thursday" = ?, "friday" = ?, "saturday" = ?, "sunday" = ?, "note" = ?
                          WHERE "row_id" = ("%s")', row_id), 
            param = list(input$title,
                         cleanFun(input$first_name),
                         cleanFun(input$last_name),
                         cleanFun(input$job_title),
                         cleanFun(input$room),
                         cleanFun(input$email),
                         cleanFun(input$phone_internal),
                         cleanFun(input$department),
                         input$monday,
                         input$tuesday,
                         input$wednesday,
                         input$thursday,
                         input$friday,
                         input$saturday,
                         input$sunday,
                         input$note
            ))
  renameData(rowid = row_id)
  removeModal()
  }

})

#################################################Delete data###################################

#Needed to keep rowID after DT refresh
cachedRowid <- reactiveValues(
  row_id = NULL
)

observeEvent(input$delete_button, {
  
  sel_row<- isolate(input$employees_table_row_last_clicked) 
  row_id <- employees_df_table()[sel_row, "row_id"] 
  cachedRowid$row_id <- row_id
})

del_row <- reactive({
    
    row_id <- cachedRowid$row_id 
  
    quary <- dbExecute(pool, sprintf('DELETE FROM "employees_df" WHERE "row_id" == ("%s")', row_id)) 
})

del_screenshot_file <- function(data_path = "www/profile_images/"){
  
  row_id <- cachedRowid$row_id 
  
  files_in_dir <- data.frame(list.files(data_path, full.names = TRUE), stringsAsFactors = FALSE)
  names(files_in_dir) <- "file_name"
  file_id <- files_in_dir %>% filter(str_detect(file_name, paste(row_id, collapse = "|")))
  file_names <- file_id[, "file_name"]
  file_name <- file_id[1, "file_name"] 
  if(!is.na(file_name)){
    lapply(file_names, function(file){file.remove(file)})
  }
  
}

observeEvent(input$delete_button, priority = 20,{
  
    showModal(
        modalDialog(id="delete_modal",
            title = "Warning",
            paste("Are you sure you want to delete this row?"),
            br(),
            br(),
            actionButton("yes_button", "Yes"),
            actionButton("no_button", "No"),
            easyClose = TRUE, footer = NULL))
  
})

observeEvent(input$yes_button, priority = 20,{
     
    del_screenshot_file()
    del_row()
    removeModal()
    
})

observeEvent(input$no_button, priority = 20,{
  removeModal()
  
})

######################################DT Output############################################


DTproxy <- dataTableProxy("employees_table") 
observeEvent(input$search_field, {
    updateSearch(DTproxy, keywords = list(global = input$search_field, columns = NULL))
})
 
output$employees_table <- DT::renderDataTable({
  
    employees_df_table <- employees_df_table() %>% select(-row_id)
    
    names(employees_df_table) <- c("Picture", "Employee", "View", "Room", "Phone Internal", "Email", "Actions")
    
    table <- DT::datatable(
        employees_df_table,
        rownames = FALSE,
        escape = FALSE,
        selection = "single",
        options = list(searching = TRUE, 
                       lengthChange = FALSE,
                       pageLength = 20,
                       autoWidth = FALSE,
                       columnDefs = (list(list(width = '50px', targets =c(0, 2, 3)),
                                          list(width = '300px', targets =c(1)),
                                          list(width = '80px', targets =c(0,6)),
                                          list(width = '130px', targets =c(4)),
                                     list(className = 'dt-center', targets = c(2,3)))),
                       initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#375a7f', 'color': '#fff'});",
                           "}")))
})

output$footer_date <- renderUI({
  
  last_modified <- max(employees_df()$date, na.rm = TRUE)
  last_date <- as.Date(last_modified, "%Y%m%d")
  last_time <- gsub(".*_","",last_modified)
  last_time <- gsub('(..)(?=.)', '\\1:', last_time, perl=TRUE)

  div(class="footer-date", HTML(paste0("Last Entry: ", last_date, " ", last_time)))
  
})            
    

}
# Run the application 
shinyApp(ui = ui, server = server)
