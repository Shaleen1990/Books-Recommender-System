
# Title Image(Open Books logo displayed here - to the left corner of the application in the left split view)
title <- tags$a(tags$img(src = "logo.png", height = 35, width = 120), strong("BooksVilla"))


# Defining the Shiny UI content
shinyUI(
  # Defining the dashboard page content
  dashboardPage(skin = "blue",
                header <- dashboardHeader(title = title,
                                          
                                          tags$li(a(href = 'http://shinyapps.company.com',
                                                    icon("power-off"),
                                                    title = "Back to Apps Home"),
                                                  class = "dropdown"),
                                          
                                          # Drop down content to the right corner of dashboard
                                          dropdownMenu
                                          (
                                             type = "messages",
                                             messageItem
                                             (
                                               from = "Cart",
                                               message = "Your cart is empty",
                                               icon = icon('shopping-cart', class = NULL, lib = "glyphicon")
                                             ),
                                             messageItem
                                             (
                                               from = "New User",
                                               message = "How do I register?",
                                               icon = icon('user', class = NULL, lib = "glyphicon")
                                               
                                             ),
                                             messageItem
                                             (
                                               from = "Support",
                                               message = "Ph. No. : 9912345678",
                                               icon = icon("life-ring")
                                               
                                             )
                                          )
                                        ),
#--------- 
# Sidebar        
#---------    
                dashboardSidebar
                (
                  sidebarMenu
                  (
                    # For selecting a book globally from the pool of books available 
                    selectizeInput
                    (
                      "Book1", "Search Book Title",choices = c("", as.character(sort(unique_books$Title)))
                    ),
                    
                    # Filters for selecting the Genre, Author, Book title
                    selectInput("Genre","Select Genre",choices = c("All", as.character(sort(unique_genres$Genre)))),
                    selectInput("Author","Select Author",choices = c("All", as.character(sort(unique_authors$Author)))),
                    selectInput("Book","Select a Book ",choices = c("All", as.character(sort(unique_books$Title)))),
                    
                    # User login
                    passwordInput("userid", "User Login", placeholder = "Enter User-ID"),
                    # LOG IN Button 
                    column(1, offset = 2,
                           div(style="display:inline-block;width:32%;text-align: center;",
                           actionButton("login_button", label = strong("Log in"), icon = icon("sign-in"), style = "background-color:skyblue"))
                          ),
                    br(),
                    br(),
                    br(),
                    column(2, offset = -7, uiOutput("login_message"))
                  )
                ),
  
#--------- 
# Body        
#---------     
                dashboardBody
                (       
                  fluidRow
                  ( 
                    # Defining the separate tab styles for 'Home' and 'Item Details'
                    tags$style(HTML(".tabbable > .nav > li > a[data-value='Home'] {background-color: #87CEEB; color:#2F4F4F}
                                    .tabbable > .nav > li > a[data-value='Item Details'] {background-color:#87CEEB;  color:	#2F4F4F}
                                    .tabbable > .nav > li[class=active]    > a {background-color: #4682B4 ; color: white}
                                    ")),
                    
                    useShinyjs(), 
                    
                    # Defining the tabset panels
                    tabsetPanel
                    ( 
                      type = "tabs",
                      id = "inTabset",
                      
                      tabPanel
                      (
                        title = strong("Home"), status = "info",value ="Home", solidHeader = TRUE, collapsible = TRUE, 
                        icon = icon("home"),
                        div(class = "rateitems",
                            uiOutput('Books_for_display')
                            )
                      ),
                      
                      tabPanel
                      (
                        title = strong("Item Details"), status = "info",value ="Item Details", solidHeader = TRUE, collapsible = TRUE,
                        icon = icon("book"),
                        fluidRow
                        (
                          box(title = code("Book Information", style = "color: darkpink ; text-align: center; font-size: 150%"),
                              width = 12, height = 300,
                              status = "primary",
                              solidHeader = F,
                              column(width = 2, height = 250,
                                     htmlOutput('CoverPic', style = "height:100; width:100")),
                              column(width = 10, height = 250,
                                     uiOutput( "BookInfo" ),
                                     tags$head(tags$style("#BookInfo{color: #DB7093;
                                                           font-size: 25px;
                                                           font-style: bold;
                                                           font-family: 'Impact', cursive;
                                                          }")),
                                     column(width = 2,
                                            offset = -8,
                                            align = 'left',
                                            uiOutput("RatingInfo")),
                                     br(),
                                     uiOutput("AuthorInfo"),
                                     br(),
                                     uiOutput("GenreInfo"),
                                     br(),
                                     uiOutput("YearInfo"))),
                        
                          # Defining 3 different box containers for displaying 3 types of recommendations
                          # For displaying ARM results: 'Frequently bought together' items with the item searched by the user
                          box(width = 12,
                              title = code("Frequently bought together:", style = "color: #1E90FF ;  background-color:#F0FFFF; text-align: center; font-size: 100%"),
                              status = "info", solidHeader = F,
                              uiOutput('ARM_book_recc_display')
                          ),
                          
                          # For displaying User based collaborative filtering results
                          box(width = 12,
                              title = code("Recommended items based on the similar users:", style = "color: green ; background-color: #F5FFFA; text-align: center; font-size: 100%"),
                              status = "success", solidHeader = F,
                              uiOutput('UBCF_book_recc_display')
                          ),
                          
                          # For displaying Item based collaborative filtering results
                          box(width = 12,
                              title = code("Recommended Items based on the similar items you purchased:", style = "color: #FF8C00 ; background-color: #FFFAF0; text-align: center; font-size: 100%"),
                              status = "warning", solidHeader = F,
                              uiOutput('IBCF_book_recc_display')
                          )
                        
                        
                      )
                    )   
                  )
                )
                )
       )
                
)              



