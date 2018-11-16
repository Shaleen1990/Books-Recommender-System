

#==========================================================
# SERVER CODE
#==========================================================

shinyServer(function(input,output,session){
  
  
  ##########################################
  # NA FUNCTIONS
  ##########################################
  # Functions created inorder to overcome the NAs displayed in the last row in the books 
  # display when the no. of books in display is not a mjultiple of 6(6- no. of books per row)
  
  #-------------------------------------
  # FUNCTION 1
  #-------------------------------------
  
  index_func <- function(n){
    x<- c()
    for(i in seq(6,n,6))
    {
      if(i%%6 == 0)
      {
        x <- append(x,i%%6)
      }
    }
    x <- append(x,6-n%%6)
    return(x)
  }
  
  
  #-------------------------------------
  # FUNCTION 2
  #-------------------------------------
  
  bookInfoTile <- function(nrows,nbooks_per_row,data_filtered){
    
    lapply(1:nrows, function(i) {
      lapply(1:nbooks_per_row, function(j) {
        
        isbn_no <- data_filtered$ISBN[(i - 1) * nbooks_per_row + j]
        #-------------------------------
        book_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Title)
        book <- book_df$Title[1]
        # CoverImg
        url_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Image.URL.L)
        url_no <- url_df$Image.URL.L[1]
        # Author
        author_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Author)
        author <- author_df$Author[1]
        # Genre
        genre_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Genre)
        genre <- genre_df$Genre[1]
        # Year
        year <- books %>% filter(books$ISBN == isbn_no) %>% select(Year)
        year <- year$Year[1]
        # RatingInfo
        avg_rating_final <- Book_rating(isbn_no)
        #-----------------------------------------
        
        a(box(width = 2, height = 270,
              div(style = "text-align:center", img(id = isbn_no,
                                                   src = data_filtered$URL[(i - 1) * nbooks_per_row + j],
                                                   width = 100, height = 150,
                                                   style="cursor:pointer;")),
              
              useShinyjs(),
              shinyjs::onclick(isbn_no,  updateTabsetPanel(session, inputId="inTabset", selected="Item Details")),
              
              shinyjs::onclick(isbn_no, book_info(isbn_no), add = TRUE),
              
              shinyjs::onclick(isbn_no, arm_recc(isbn_no), add = TRUE),
              
              div(style = "text-align:center; color: #999999; font-size: 80%", data_filtered$Author[(i - 1) * nbooks_per_row + j]),
              div(style = "text-align:center", strong(data_filtered$Title[(i - 1) * nbooks_per_row + j])),
              
              div(style = "text-align:center; color : #f0ad4e",
                  ratingInput(paste0("bookRating_",isbn_no), label="", class = "rating",
                              # dataFilled="fa fa-star",
                              # dataEmpty="fa fa-star-o",
                              dataStart=0,
                              dataStop=5,
                              dataStep = 1,
                              dataFractions=2,
                              disabled=TRUE,
                              value= avg_rating_final))))
        
      })})
    
  }
  
  
  #-------------------------------------
  # FUNCTION 3
  #-------------------------------------  
  
  bookInfoTile_withIndex <- function(nrows,nbooks_per_row,data_filtered){
    
    index_list <- index_func(nrow(data_filtered))
    lapply(1:nrows, function(i) {
      lapply(1:(nbooks_per_row - index_list[i]), function(j) {
        
        isbn_no <- data_filtered$ISBN[(i - 1) * nbooks_per_row + j]
        #-------------------------------
        book_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Title)
        book <- book_df$Title[1]
        # CoverImg
        url_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Image.URL.L)
        url_no <- url_df$Image.URL.L[1]
        # Author
        author_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Author)
        author <- author_df$Author[1]
        # Genre
        genre_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Genre)
        genre <- genre_df$Genre[1]
        # Year
        year <- books %>% filter(books$ISBN == isbn_no) %>% select(Year)
        year <- year$Year[1]
        # RatingInfo
        avg_rating_final <- Book_rating(isbn_no)
        #-----------------------------------------
        
        
        a(box(width = 2, height = 270,
              div(style = "text-align:center", img(id = isbn_no,
                                                   src = data_filtered$URL[(i - 1) * nbooks_per_row + j],
                                                   width = 100, height = 150,
                                                   style="cursor:pointer;")),
              
              useShinyjs(),
              shinyjs::onclick(isbn_no,  updateTabsetPanel(session, inputId="inTabset", selected="Item Details")),
              
              shinyjs::onclick(isbn_no, book_info(isbn_no), add = TRUE),
              
              shinyjs::onclick(isbn_no, arm_recc(isbn_no), add = TRUE),
              
              div(style = "text-align:center; color: #999999; font-size: 80%", data_filtered$Author[(i - 1) * nbooks_per_row + j]),
              div(style = "text-align:center", strong(data_filtered$Title[(i - 1) * nbooks_per_row + j])),
              
              div(style = "text-align:center; color : #f0ad4e",
                  ratingInput(paste0("bookRating_",isbn_no), label="", class = "rating",
                              # dataFilled="fa fa-star",
                              # dataEmpty="fa fa-star-o",
                              dataStart=0,
                              dataStop=5,
                              dataStep = 1,
                              dataFractions=2,
                              disabled=TRUE,
                              value= avg_rating_final))))
        
      })})
  }
  
  
  
  #--------------------------------------------------------------------------------
  # Item details_Book info display_REUSABLE CODE (on click of an image)
  #--------------------------------------------------------------------------------
  # Everytime when you tap on any book in the home page, that should redirect you to the 
  # Item_details page and the following information pertaining to that book should be displayed 
  # in Item Details tab
  book_info <- function(isbn_no)
  {
    # Book name display
    book_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Title)
    book <- book_df$Title[1]
    output$BookInfo <- renderText(as.character(book))
    # CoverImg display
    url_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Image.URL.L)
    url_no <- url_df$Image.URL.L[1]
    output$CoverPic <- renderUI(img(src = url_no, style = "width: 150px; height: 220px"))
    # Author name display
    author_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Author)
    author <- author_df$Author[1]
    output$AuthorInfo <- renderText(paste(code("Author  :", style = "color: darkpink; font-size: 120%"),author))
    # Genre name display
    genre_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Genre)
    genre <- genre_df$Genre[1]
    output$GenreInfo <- renderText(paste(code("Genre  :", style = "color: darkpink; font-size: 120%"),genre))
    # Year display
    year <- books %>% filter(books$ISBN == isbn_no) %>% select(Year)
    year <- year$Year[1]
    output$YearInfo <- renderText(paste(code("Year  :", style = "color: darkpink; font-size: 120%"),year))
    # RatingInfo inplay with stars
    avg_rating_final <- Book_rating(isbn_no)
    output$RatingInfo <- renderUI(
      div(style = "text-align:center; color : #f0ad4e",
          ratingInput(paste0("bookRating_",input$book1), label="", class = "rating",
                      dataStart=0,
                      dataStop=5,
                      dataStep = 1,
                      dataFractions=2,
                      disabled=TRUE,
                      value= avg_rating_final)))
  }
  
  
  #--------------------------------------------------------------------------------
  # ARM_Book_Recc_display_REUSABLE CODE (on click of an image)
  #--------------------------------------------------------------------------------
  # Along with the Book information, ARM results should also be displayed when the user 
  # logs in and select any of the available books from the app
  
  arm_recc <- function(isbn_no)
  {
      if(input$userid %in% books$User.ID)
      {
        arm_recc_items <- arm4(id = input$userid, isbn = as.character(isbn_no))
        if(class(arm_recc_items) != "try-error")
        {
        data_filtered <- books %>% filter(books$ISBN %in% arm_recc_items)
        data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
        output$ARM_book_recc_display <- renderUI( 
          {
            nbooks <- nrow(data_filtered)
            nbooks_per_row <- nrow(data_filtered)
            nrows <- 1
            
            lapply(1:nrows, function(i) {
              list(fluidRow(lapply(1:nbooks_per_row, function(j) {
                
                isbn_no <- data_filtered$ISBN[(i - 1) * nbooks_per_row + j]
                #-------------------------------
                book_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Title)
                book <- book_df$Title[1]
                # CoverImg
                url_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Image.URL.L)
                url_no <- url_df$Image.URL.L[1]
                # Author
                author_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Author)
                author <- author_df$Author[1]
                # Genre
                genre_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Genre)
                genre <- genre_df$Genre[1]
                # Year
                year <- books %>% filter(books$ISBN == isbn_no) %>% select(Year)
                year <- year$Year[1]
                # RatingInfo
                avg_rating_final <- Book_rating(isbn_no)
                #-----------------------------------------
                
                #sr = cover_name(data_filtered$URL[(i - 1) * nbooks_per_row + j]) 
                
                a(box(width = 2, height = 270,
                      div(style = "text-align:center", img(id = isbn_no,
                                                           src = data_filtered$URL[(i - 1) * nbooks_per_row + j], 
                                                           width = 100, height = 150,
                                                           style="cursor:pointer;")),
                      
                      useShinyjs(),
                      shinyjs::onclick(isbn_no,  updateTabsetPanel(session, inputId="inTabset", selected="Item Details")),
                      
                      shinyjs::onclick(isbn_no, book_info(isbn_no), add = TRUE),
                      
                      shinyjs::onclick(isbn_no, arm_recc(isbn_no), add = TRUE),
                      
                      div(style = "text-align:center; color: #999999; font-size: 80%", data_filtered$Author[(i - 1) * nbooks_per_row + j]),
                      div(style = "text-align:center", strong(data_filtered$Title[(i - 1) * nbooks_per_row + j])),
                      
                      div(style = "text-align:center; color : #f0ad4e",
                          ratingInput(paste0("bookRating_",isbn_no), label="", class = "rating",
                                      dataStart=0,
                                      dataStop=5,
                                      dataStep = 1,
                                      dataFractions=2,
                                      disabled=TRUE,
                                      value= avg_rating_final))),
                  if(j != nrow(data_filtered))
                  {
                    column(width = 1,
                           br(),
                           br(),
                           br(),
                           br(),
                           div(style = "text-align:center",
                               icon("plus", lib = "glyphicon")))
                  }
                  
                  
                )
                
              })))
            })
            
          }
        )
        }
        else
        {
          output$ARM_book_recc_display <- renderUI({
            column(8, offset = 0.5,
                   img(src = "sad_face.jpeg",  # A sad face image is displayed when the algorithm doesnt produce any recommendations
                       width = 180, height = 150))
          })
          
        }
        
        
        
      }
    
  }
  
  
  
#------------------------------------    
# Searchbar actions
#------------------------------------   
# When a user selects any book globally from the 1st dropdown in the left side bar layout:
  
  observe(
    {
      if(input$Book1!= '')
      {
        data_filtered <- books %>% filter(books$Title == input$Book1)
        data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
        output$Books_for_display <- renderUI(
          {
            box(width = 2, height = 250,
                div(style = "text-align:center",
                    img(src = data_filtered$URL[1],
                        width = 100, height = 150,
                        style = "max-height:150")),
                div(style = "text-align:center; color: #999999; font-size: 80%", data_filtered$Author[1]),
                div(style = "text-align:center", strong(data_filtered$Title[1]))
            )    
          })
      }
    })
  
  
#------------------------------------    
# User login actions
#------------------------------------ 
  
  observeEvent(input$login_button,
     {
       # If the existing user logs in
       if(input$userid %in% books$User.ID)
       {
         output$login_message <- renderText(paste(strong("U r successfully logged in!"), br(),"User-ID: ", strong(input$userid)))
       }
       # If an invalid user(not an existing user) tries to log in
       else
       {
         output$login_message <- renderText(paste0("Sorry! ",strong("Invalid user "),input$userid))
       }
       
     }
  )
  
  observe(
    {
      # When the user id is NULL
      if(input$userid == '')
      {
        output$login_message <- renderText("Pls Login 2 get ur Recommendations")
        output$ARM_book_recc_display <- renderText('')
        output$UBCF_book_recc_display <- renderText('')
        output$IBCF_book_recc_display <- renderText('')
      }
    }
  )
  
  
  
  
#----------------------------------------   
# Sidebar layout_dropdown Filter actions
#----------------------------------------  
  
    observe(
      {
        # When only Genre is selected
        if(input$Genre != "All")
        {
          authors_filtered <- books %>% filter(books$Genre == input$Genre) %>% select(Author)
          books_filtered <- books %>% filter(books$Genre == input$Genre) %>% select(Title)
          updateSelectInput(session,"Author","Select Author",choices = c("All", as.character(sort(unique(authors_filtered)$Author))))
          updateSelectInput(session,"Book","Select a Book",choices = c("All", as.character(sort(unique(books_filtered)$Title))))
        }
        else
        {
          # Genre is the 1st and top filter in the side bar layout, 
          # So, when Genre='All', then Author and Book should also be updated to 'All'
          updateSelectInput(session,"Author","Select Author",choices = c("All", as.character(sort(unique_authors$Author))))
          updateSelectInput(session,"Book","Select a Book",choices = c("All", as.character(sort(unique_books$Title))))
        }
      })

    observe(
      {
        # When only Author is selected:
        if(input$Author != "All")
        {
          # When both Author and Genre are selected:
          if(input$Genre != "All")
          {
            books_filtered <- books %>% filter(books$Author == input$Author & books$Genre == input$Genre) %>% select(Title)
            updateSelectInput(session,"Book","Select a Book",choices = c("All", as.character(sort(unique(books_filtered)$Title))))
          }
          else
          {
            books_filtered <- books %>% filter(books$Author == input$Author) %>% select(Title)
            updateSelectInput(session,"Book","Select a Book",choices = c("All", as.character(sort(unique(books_filtered)$Title))))
          }
          
        }
        else
        {
          if(input$Genre != "All")
          {
            books_filtered2 <- books %>% filter(books$Genre == input$Genre) %>% select(Title)
            updateSelectInput(session,"Book","Select a Book",choices = c("All", as.character(sort(unique(books_filtered2)$Title))))
          }
          else
          {
            updateSelectInput(session,"Book","Select a Book",choices = c("All", as.character(sort(unique_books$Title))))
          }
          
        }
      })
    

    
#------------------------------------     
# Homepage_books view_filtered
#------------------------------------     
    
    # Displaying only those books filtered from the options provided under dropdowns in side bar layout
    
    observe(
      {
        # When all the 3 are selected by the user:
        if(input$Genre != "All" & input$Author != "All" & input$Book != "All")
        {
          data_filtered <- books %>% filter(books$Genre == input$Genre & books$Author == input$Author & books$Title == input$Book)
          data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
         
          output$Books_for_display <- renderUI(
            {
              nbooks_per_row <- 0
              nbooks <- nrow(data_filtered)
              if(nbooks%%6 == 0){
                nbooks_per_row = 6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks%%6 != 0 & nbooks < 6){
                nbooks_per_row = nbooks%%6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks > 6){
                nbooks_per_row = 6
                nrows <- nbooks/6 + 1
                bookInfoTile_withIndex(nrows,nbooks_per_row,data_filtered)
                
              }
            })
        }
        

        # When only Genre and Author are selected by the user:
        else if(input$Genre != "All" & input$Author != "All")
        {
          data_filtered <- books %>% filter(books$Genre == input$Genre & books$Author == input$Author)
          data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
          data_filtered <- data_filtered[order(data_filtered$Title),]
          
          output$Books_for_display <- renderUI(
            {
              nbooks_per_row <- 0
              nbooks <- nrow(data_filtered)
              if(nbooks%%6 == 0){
                nbooks_per_row = 6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks%%6 != 0 & nbooks < 6){
                nbooks_per_row = nbooks%%6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks > 6){
                nbooks_per_row = 6
                nrows <- nbooks/6 + 1
                bookInfoTile_withIndex(nrows,nbooks_per_row,data_filtered)
                
              }
            })
        }
        
        # When only Genre and Book title are selected by the user:
        else if(input$Genre != "All" & input$Book != "All")
        {
          data_filtered <- books %>% filter(books$Genre == input$Genre & books$Title == input$Book)
          data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
          data_filtered <- data_filtered[order(data_filtered$Title),]
          
          output$Books_for_display <- renderUI(
            {
              nbooks_per_row <- 0
              nbooks <- nrow(data_filtered)
              if(nbooks%%6 == 0){
                nbooks_per_row = 6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks%%6 != 0 & nbooks < 6){
                nbooks_per_row = nbooks%%6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks > 6){
                nbooks_per_row = 6
                nrows <- nbooks/6 + 1
                bookInfoTile_withIndex(nrows,nbooks_per_row,data_filtered)
                
              }
            })
        }
        
        # When only Author and Book title are selected by the user:
        else if(input$Author != "All" & input$Book != "All")
        {
          data_filtered <- books %>% filter(books$Author == input$Author & books$Title == input$Book)
          data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
          data_filtered <- data_filtered[order(data_filtered$Title),]
          
          output$Books_for_display <- renderUI(
            {
              nbooks_per_row <- 0
              nbooks <- nrow(data_filtered)
              if(nbooks%%6 == 0){
                nbooks_per_row = 6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks%%6 != 0 & nbooks < 6){
                nbooks_per_row = nbooks%%6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks > 6){
                nbooks_per_row = 6
                nrows <- nbooks/6 + 1
                bookInfoTile_withIndex(nrows,nbooks_per_row,data_filtered)
                
              }
            })
        }
        
        # When only Genre is selected by the user:
        else if(input$Genre != "All")
        {
          data_filtered <- books %>% filter(books$Genre == input$Genre)
          data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
          data_filtered <- data_filtered[order(data_filtered$Title),]
          
            output$Books_for_display <- renderUI(
              {
                nbooks_per_row <- 0
                nbooks <- nrow(data_filtered)
                if(nbooks%%6 == 0){
                  nbooks_per_row = 6
                  nrows <- floor(nbooks/nbooks_per_row)
                  bookInfoTile(nrows,nbooks_per_row,data_filtered)
                  
                }
                else if(nbooks%%6 != 0 & nbooks < 6){
                  nbooks_per_row = nbooks%%6
                  nrows <- floor(nbooks/nbooks_per_row)
                  bookInfoTile(nrows,nbooks_per_row,data_filtered)
                  
                }
                else if(nbooks > 6){
                  nbooks_per_row = 6
                  nrows <- nbooks/6 + 1
                  bookInfoTile_withIndex(nrows,nbooks_per_row,data_filtered)
                  
                }
              })
        }
        
        # When only the Author is selected:
        else if(input$Author != "All")
        {
          data_filtered <- books %>% filter(books$Author == input$Author)
          data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
          data_filtered <- data_filtered[order(data_filtered$Title),]
          
          output$Books_for_display <- renderUI(
            {
              nbooks_per_row <- 0
              nbooks <- nrow(data_filtered)
              if(nbooks%%6 == 0){
                nbooks_per_row = 6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks%%6 != 0 & nbooks < 6){
                nbooks_per_row = nbooks%%6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks > 6){
                nbooks_per_row = 6
                nrows <- nbooks/6 + 1
                bookInfoTile_withIndex(nrows,nbooks_per_row,data_filtered)
                
              }
            })
        }
        
        
        # When only the Book Title is selected:
        else if(input$Book != "All")
        {
          data_filtered <- books %>% filter(books$Title == input$Book)
          data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
          data_filtered <- data_filtered[order(data_filtered$Title),]
          
          output$Books_for_display <- renderUI(
            {
              nbooks_per_row <- 0
              nbooks <- nrow(data_filtered)
              if(nbooks%%6 == 0){
                nbooks_per_row = 6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks%%6 != 0 & nbooks < 6){
                nbooks_per_row = nbooks%%6
                nrows <- floor(nbooks/nbooks_per_row)
                bookInfoTile(nrows,nbooks_per_row,data_filtered)
                
              }
              else if(nbooks > 6){
                nbooks_per_row = 6
                nrows <- nbooks/6 + 1
                bookInfoTile_withIndex(nrows,nbooks_per_row,data_filtered)
                
              }
            })
        }
        
        
        #------------------------------------------------------------
        # Homepage popular books display(books bought by many people)
        #------------------------------------------------------------
        # These(popular books) are the books read by most of the users
        # These should be displayed only when the user is not logged in and when no book is selected
        else if(input$Genre == "All" & input$Author == "All" & input$Book == "All" & input$Book1 == '' & input$userid == '')
        {
          data_filtered <- fn$sqldf("select Title, ISBN, Title, Author, URL, count(Rating)
                                    from books group by Title order by count(Rating) desc")
          output$Books_for_display <- renderUI(
            {
              nrows <- 3
              nbooks_per_row <- 6
              
              bookInfoTile(nrows,nbooks_per_row,data_filtered)
            })
        }
        
        
    })
        
    
#-------------------------------------
# GENERAL RECOMMENDATIONS UPON LOGIN
#-------------------------------------
        
    observe(
      {
        if(input$login_button & input$userid %in% books$User.ID)
        {
          
          #--------------------------------------------
          # GRM Reccomendations
          #--------------------------------------------\
          # Calling the GRM function which returns the list of book ISBNs
          grm_recc_items_list <- grm(id = input$userid)
          # Filtering whole data with only those books in the list
          data_filtered_grm <- books %>% filter(books$ISBN %in% grm_recc_items_list)
          # Removing the duplicates if any
          data_filtered_grm <- data_filtered_grm[!duplicated(data_filtered_grm[,c('Title')]),]
          index_list <- index_func(nrow(data_filtered_grm))
          # Displaying those books
          output$Books_for_display <- renderUI(
            fluidRow(
              {
                nbooks <- nrow(data_filtered_grm)
                nbooks_per_row <- 6
                nrows <- floor(nbooks/nbooks_per_row)
                box(title = code(paste0("Hi!",input$userid,',',"Inspired by your purchases:"), style = "color: darkpink ; text-align: center; font-size: 120%"),
                    width = 12,
                    status = "primary",
                    solidHeader = F,
                    
                    lapply(1:nrows, function(i) {
                      list(fluidRow(lapply(1:(nbooks_per_row - index_list[i]), function(j) {
                        
                        isbn_no <- data_filtered_grm$ISBN[(i - 1) * nbooks_per_row + j]
                        
                        #-------------------------------
                        book_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Title)
                        book <- book_df$Title[1]
                        # CoverImg
                        url_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Image.URL.L)
                        url_no <- url_df$Image.URL.L[1]
                        # Author
                        author_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Author)
                        author <- author_df$Author[1]
                        # Genre
                        genre_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Genre)
                        genre <- genre_df$Genre[1]
                        # Year
                        year <- books %>% filter(books$ISBN == isbn_no) %>% select(Year)
                        year <- year$Year[1]
                        # RatingInfo
                        avg_rating_final <- Book_rating(isbn_no)
                        #-----------------------------------------
                        
                        a(box(width = 2, height = 270,
                              div(style = "text-align:center", img(id = isbn_no,
                                                                   src = data_filtered_grm$URL[(i - 1) * nbooks_per_row + j], 
                                                                   width = 100, height = 150,
                                                                   style="cursor:pointer;")),
                              
                              useShinyjs(),
                              shinyjs::onclick(isbn_no,  updateTabsetPanel(session, inputId="inTabset", selected="Item Details")),
                              
                              shinyjs::onclick(isbn_no, book_info(isbn_no), add = TRUE),
                              
                              shinyjs::onclick(isbn_no, arm_recc(isbn_no), add = TRUE),
                              
                              
                              
                              div(style = "text-align:center; color: #999999; font-size: 80%", data_filtered_grm$Author[(i - 1) * nbooks_per_row + j]),
                              div(style = "text-align:center", strong(data_filtered_grm$Title[(i - 1) * nbooks_per_row + j])),
                              
                              div(style = "text-align:center; color : #f0ad4e",
                                  ratingInput(paste0("bookRating_",isbn_no), label="", class = "rating",
                                              # dataFilled="fa fa-star",
                                              # dataEmpty="fa fa-star-o",
                                              dataStart=0,
                                              dataStop=5,
                                              dataStep = 1,
                                              dataFractions=2,
                                              disabled=TRUE,
                                              value= avg_rating_final))))
                      })))
                    }))
                
              }))
          
          
        }
      }
    )
        
    
    
#------------------------------------    
# Home page view filtered_ search book
#------------------------------------
    # When any book is selected by the user, only that particular book should be displayed
    # in the home page oveerwriting the previous view.
    
     observe(
       {
         if(input$Book1 != '')
         {
           data_filtered <- books %>% filter(books$Title == input$Book1)
           data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
           data_filtered <- data_filtered[order(data_filtered$Title),]
           
           output$Books_for_display <- renderUI(
             {
               nbooks_per_row <- 0
               nbooks <- nrow(data_filtered)
               if(nbooks%%6 == 0){
                 nbooks_per_row = 6
                 nrows <- floor(nbooks/nbooks_per_row)
                 bookInfoTile(nrows,nbooks_per_row,data_filtered)
                 
               }
               else if(nbooks%%6 != 0 & nbooks < 6){
                 nbooks_per_row = nbooks%%6
                 nrows <- floor(nbooks/nbooks_per_row)
                 bookInfoTile(nrows,nbooks_per_row,data_filtered)
                 
               }
               else if(nbooks > 6){
                 nbooks_per_row = 6
                 nrows <- nbooks/6 + 1
                 bookInfoTile_withIndex(nrows,nbooks_per_row,data_filtered)
                 
               }
                   })
               }
       })    
  
#------------------------------------    
# Item Details_Tab : Book Info display
#------------------------------------

     #---------------------------------------------------
     # Book Info display with filter dropdown inputs
     #---------------------------------------------------
     # Displaying the appropriate book information of the book selected by the user 
     #from the home page or directky fromthe side bar filters
    
     observe(
       {
         # When  all the 3 filters are selected by the user
         if(input$Book != 'All' & input$Author != 'All' & input$Genre != 'All')
         {
           output$BookInfo <- renderText(input$Book)
           # CoverImg
           url_df <- books %>% filter(books$Title %in% input$Book) %>% select(Image.URL.L)
           url_no <- url_df$Image.URL.L[1]
           output$CoverPic <- renderUI(img(src = url_no, style = "width: 150px; height: 220px"))
           # Author
           output$AuthorInfo <- renderText(paste(code("Author  :", style = "color: darkpink; font-size: 120%"),input$Author))
           # Genre
           output$GenreInfo <- renderText(paste(code("Genre  :", style = "color: darkpink; font-size: 120%"),input$Genre))
           # Year
           year <- books %>% filter(books$Title %in% input$Book) %>% select(Year)
           year <- year$Year[1]
           output$YearInfo <- renderText(paste(code("Year  :", style = "color: darkpink; font-size: 120%"),year))
           # RatingInfo
           isbn_df <- books %>% filter(books$Title %in% input$Book) %>% select(ISBN)
           isbn_no <- isbn_df$ISBN[1]
           avg_rating_final <- Book_rating(isbn_no)
           output$RatingInfo <- renderUI(
             div(style = "text-align:center; color : #f0ad4e",
                 ratingInput(paste0("bookRating_",input$book), label="", class = "rating",
                             # dataFilled="fa fa-star",
                             # dataEmpty="fa fa-star-o",
                             dataStart=0,
                             dataStop=5,
                             dataStep = 1,
                             dataFractions=2,
                             disabled=TRUE,
                             value= avg_rating_final)))
           
         }
         # When only Book title and the Author are selected:
         else if(input$Book != 'All' & input$Author != 'All')
         {
           output$BookInfo <- renderText(input$Book)
           # CoverImg
           url_df <- books %>% filter(books$Title %in% input$Book) %>% select(Image.URL.L)
           url_no <- url_df$Image.URL.L[1]
           output$CoverPic <- renderUI(img(src = url_no, style = "width: 150px; height: 220px"))
           # Author
           output$AuthorInfo <- renderText(paste(code("Author  :", style = "color: darkpink; font-size: 120%"),input$Author))
           # Genre
           genre_df <- books %>% filter(books$Title == input$Book) %>% select(Genre)
           genre <- genre_df$Genre[1]
           output$GenreInfo <- renderText(paste(code("Genre  :", style = "color: darkpink; font-size: 120%"),genre))
           # Year
           year <- books %>% filter(books$Title %in% input$Book) %>% select(Year)
           year <- year$Year[1]
           output$YearInfo <- renderText(paste(code("Year  :", style = "color: darkpink; font-size: 120%"),year))
           # RatingInfo
           isbn_df <- books %>% filter(books$Title %in% input$Book) %>% select(ISBN)
           isbn_no <- isbn_df$ISBN[1]
           avg_rating_final <- Book_rating(isbn_no)
           output$RatingInfo <- renderUI(
             div(style = "text-align:center; color : #f0ad4e",
                 ratingInput(paste0("bookRating_",input$book), label="", class = "rating",
                             # dataFilled="fa fa-star",
                             # dataEmpty="fa fa-star-o",
                             dataStart=0,
                             dataStop=5,
                             dataStep = 1,
                             dataFractions=2,
                             disabled=TRUE,
                             value= avg_rating_final)))
           
         }
         # When only Book title and Genre are selected
         else if(input$Book != 'All' & input$Genre != 'All')
         {
           output$BookInfo <- renderText(input$Book)
           # CoverImg
           url_df <- books %>% filter(books$Title %in% input$Book) %>% select(Image.URL.L)
           url_no <- url_df$Image.URL.L[1]
           output$CoverPic <- renderUI(img(src = url_no, style = "width: 150px; height: 220px"))
           # Author
           author_df <- books %>% filter(books$Title == input$Book) %>% select(Author)
           author <- author_df$Author[1]
           output$AuthorInfo <- renderText(paste(code("Author  :", style = "color: darkpink; font-size: 120%"),author))
           # Genre
           output$GenreInfo <- renderText(paste(code("Genre  :", style = "color: darkpink; font-size: 120%"),input$Genre))
           # Year
           year <- books %>% filter(books$Title %in% input$Book) %>% select(Year)
           year <- year$Year[1]
           output$YearInfo <- renderText(paste(code("Year  :", style = "color: darkpink; font-size: 120%"),year))
           # RatingInfo
           isbn_df <- books %>% filter(books$Title %in% input$Book) %>% select(ISBN)
           isbn_no <- isbn_df$ISBN[1]
           avg_rating_final <- Book_rating(isbn_no)
           output$RatingInfo <- renderUI(
             div(style = "text-align:center; color : #f0ad4e",
                 ratingInput(paste0("bookRating_",input$book), label="", class = "rating",
                             # dataFilled="fa fa-star",
                             # dataEmpty="fa fa-star-o",
                             dataStart=0,
                             dataStop=5,
                             dataStep = 1,
                             dataFractions=2,
                             disabled=TRUE,
                             value= avg_rating_final)))
           
         }
         # When only the Book Title is selected:
         else if(input$Book != 'All')
         {
           output$BookInfo <- renderText(input$Book)
           # CoverImg
           url_df <- books %>% filter(books$Title %in% input$Book) %>% select(Image.URL.L)
           url_no <- url_df$Image.URL.L[1]
           output$CoverPic <- renderUI(img(src = url_no, style = "width: 150px; height: 220px"))
           # Author
           author_df <- books %>% filter(books$Title == input$Book) %>% select(Author)
           author <- author_df$Author[1]
           output$AuthorInfo <- renderText(paste(code("Author  :", style = "color: darkpink; font-size: 120%"),author))
           # Genre
           genre_df <- books %>% filter(books$Title == input$Book) %>% select(Genre)
           genre <- genre_df$Genre[1]
           output$GenreInfo <- renderText(paste(code("Genre  :", style = "color: darkpink; font-size: 120%"),genre))
           # Year
           year <- books %>% filter(books$Title %in% input$Book) %>% select(Year)
           year <- year$Year[1]
           output$YearInfo <- renderText(paste(code("Year  :", style = "color: darkpink; font-size: 120%"),year))
           # RatingInfo
           isbn_df <- books %>% filter(books$Title %in% input$Book) %>% select(ISBN)
           isbn_no <- isbn_df$ISBN[1]
           avg_rating_final <- Book_rating(isbn_no)
           output$RatingInfo <- renderUI(
             div(style = "text-align:center; color : #f0ad4e",
                 ratingInput(paste0("bookRating_",input$book), label="", class = "rating",
                             # dataFilled="fa fa-star",
                             # dataEmpty="fa fa-star-o",
                             dataStart=0,
                             dataStop=5,
                             dataStep = 1,
                             dataFractions=2,
                             disabled=TRUE,
                             value= avg_rating_final)))
           
         }
         
         else
         {
           # When nothing is selected, book information should be blank
           output$BookInfo <- renderText('')
           output$CoverPic <- renderText('')
           output$RatingInfo <- renderText('')
           output$AuthorInfo <- renderText('')
           output$GenreInfo <- renderText('')
           output$YearInfo <- renderText('')
           
         }
       
        })
    
    
      #-----------------------------------------------------------------------------
      # Book Info display with search a book- global book filter in side bar layout
      #-----------------------------------------------------------------------------
      observe(
        {
          if(input$Book1 != '')
          {
            output$BookInfo <- renderText(input$Book1)
            # CoverImg
            url_df <- books %>% filter(books$Title %in% input$Book1) %>% select(Image.URL.L)
            url_no <- url_df$Image.URL.L[1]
            output$CoverPic <- renderUI(img(src = url_no, style = "width: 150px; height: 220px"))
            # Author
            author_df <- books %>% filter(books$Title == input$Book1) %>% select(Author)
            author <- author_df$Author[1]
            output$AuthorInfo <- renderText(paste(code("Author  :", style = "color: darkpink; font-size: 120%"),author))
            # Genre
            genre_df <- books %>% filter(books$Title == input$Book1) %>% select(Genre)
            genre <- genre_df$Genre[1]
            output$GenreInfo <- renderText(paste(code("Genre  :", style = "color: darkpink; font-size: 120%"),genre))
            # Year
            year <- books %>% filter(books$Title %in% input$Book1) %>% select(Year)
            year <- year$Year[1]
            output$YearInfo <- renderText(paste(code("Year  :", style = "color: darkpink; font-size: 120%"),year))
            # RatingInfo
            isbn_df <- books %>% filter(books$Title %in% input$Book1) %>% select(ISBN)
            isbn_no <- isbn_df$ISBN[1]
            avg_rating_final <- Book_rating(isbn_no)
            output$RatingInfo <- renderUI(
              div(style = "text-align:center; color : #f0ad4e",
                  ratingInput(paste0("bookRating_",input$book1), label="", class = "rating",
                              dataStart=0,
                              dataStop=5,
                              dataStep = 1,
                              dataFractions=2,
                              disabled=TRUE,
                              value= avg_rating_final)))
          }
          else
          {
            output$BookInfo <- renderText('')
            output$CoverPic <- renderText('')
            output$RatingInfo <- renderText('')
            output$AuthorInfo <- renderText('')
            output$GenreInfo <- renderText('')
            output$YearInfo <- renderText('')
            
          }
        })
          
    

     
 #------------------------------------    
 # ARM_recc_display
 #------------------------------------     
    # Displaying the ARM results when a book is selected by the user upon login
    observe(
      {
        if((input$Book != "All" & input$login_button & input$userid %in% books$User.ID) | (input$Book1 != '' & input$login_button & input$userid %in% books$User.ID))
        {
          #-------------------------
          #arm_recc_items
          #-------------------------
          isbn_df <- books %>% filter(books$Title == input$Book | books$Title == input$Book1) %>% select(ISBN)
          isbn_name <- as.character(isbn_df$ISBN[1])
          arm_recc_items <- arm4(id = input$userid, isbn = isbn_name)
          if(class(arm_recc_items) != "try-error")
          {
            data_filtered <- books %>% filter(books$ISBN %in% arm_recc_items)
            data_filtered <- data_filtered[!duplicated(data_filtered[,c('Title')]),]
            output$ARM_book_recc_display <- renderUI(
              {
                nbooks <- nrow(data_filtered)
                nbooks_per_row <- nrow(data_filtered)
                nrows <- 1
                
                lapply(1:nrows, function(i) {
                  list(fluidRow(lapply(1:nbooks_per_row, function(j) {
                    
                    isbn_no <- data_filtered$ISBN[(i - 1) * nbooks_per_row + j]
                    
                    #-------------------------------
                    book_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Title)
                    book <- book_df$Title[1]
                    # CoverImg
                    url_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Image.URL.L)
                    url_no <- url_df$Image.URL.L[1]
                    # Author
                    author_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Author)
                    author <- author_df$Author[1]
                    # Genre
                    genre_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Genre)
                    genre <- genre_df$Genre[1]
                    # Year
                    year <- books %>% filter(books$ISBN == isbn_no) %>% select(Year)
                    year <- year$Year[1]
                    # RatingInfo
                    avg_rating_final <- Book_rating(isbn_no)
                    #-----------------------------------------
                    
                    a(box(width = 2, height = 270,
                          div(style = "text-align:center", img(id = isbn_no,
                                                               src = data_filtered$URL[(i - 1) * nbooks_per_row + j], 
                                                               width = 100, height = 150,
                                                               style="cursor:pointer;")),
                          
                          useShinyjs(),
                          shinyjs::onclick(isbn_no,  updateTabsetPanel(session, inputId="inTabset", selected="Item Details")),
                          
                          shinyjs::onclick(isbn_no, book_info(isbn_no), add = TRUE),
                          
                          shinyjs::onclick(isbn_no, arm_recc(isbn_no), add = TRUE),
                          
                          div(style = "text-align:center; color: #999999; font-size: 80%", data_filtered$Author[(i - 1) * nbooks_per_row + j]),
                          div(style = "text-align:center", strong(data_filtered$Title[(i - 1) * nbooks_per_row + j])),
                          
                          div(style = "text-align:center; color : #f0ad4e",
                              ratingInput(paste0("bookRating_",isbn_no), label="", class = "rating",
                                          dataStart=0,
                                          dataStop=5,
                                          dataStep = 1,
                                          dataFractions=2,
                                          disabled=TRUE,
                                          value= avg_rating_final))),
                      if(j != nrow(data_filtered))
                      {
                        column(width = 1,
                               br(),
                               br(),
                               br(),
                               br(),
                               #width = 1, height = 1,
                               div(style = "text-align:center",
                                   icon("plus", lib = "glyphicon")))
                      }
                      
                      
                    )
                    
                  })))
                })
                
              }
            )
          }
          else
          {
            # When the user doesnt login or when when the user doesnt select any book, 
            # ARM results cant be processed and thus should be blank
            output$ARM_book_recc_display <- renderUI({
                                                    column(8, offset = 0.5,
                                                    img(src = "sad_face.jpeg",
                                                    width = 180, height = 150))
                                                    })
                                                
          }
         
        } 
        
      })
    
    
    
#-----------------------------------------------------------    
# Personal Recommendations display_ Collaborative filtering
#-----------------------------------------------------------     
      # Collaborative filtering results(UBCF as well as IBCF) should be displayed only when the user logs in.
      observe(
        {
          if(input$login_button & input$userid %in% books$User.ID)
          {
            #-------------------------
            # UBCF_recc_items
            #-------------------------
            
            # Calling the UBCF function which returns a named list of books
            ubcf_recc_items <- ubcf(id = input$userid)
            
            # Converting to a normal proper list format
            if(ubcf_recc_items[input$userid] != 'character(0)')
            {
              
            ubcf_recc_items_list <- c()
            for (i in 1:5)
            {
              ubcf_recc_items_list <- c(ubcf_recc_items_list, ubcf_recc_items[[input$userid]][i])
            }
            
            # Filtering the data with only those books
            data_filtered_ubcf <- books %>% filter(books$ISBN %in% ubcf_recc_items_list)
            data_filtered_ubcf <- data_filtered_ubcf[!duplicated(data_filtered_ubcf[,c('Title')]),]
            
            # Displaying the books in Items tab under the corresponding box allocated for it in UI page:
            output$UBCF_book_recc_display <- renderUI(
              {
                nbooks <- nrow(data_filtered_ubcf)
                nbooks_per_row <- nrow(data_filtered_ubcf)
                nrows <- 1
                
                lapply(1:nrows, function(i) {
                  list(fluidRow(lapply(1:nbooks_per_row, function(j) {
                    
                    isbn_no <- data_filtered_ubcf$ISBN[(i - 1) * nbooks_per_row + j]
                    
                    #-------------------------------
                    book_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Title)
                    book <- book_df$Title[1]
                    # CoverImg
                    url_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Image.URL.L)
                    url_no <- url_df$Image.URL.L[1]
                    # Author
                    author_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Author)
                    author <- author_df$Author[1]
                    # Genre
                    genre_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Genre)
                    genre <- genre_df$Genre[1]
                    # Year
                    year <- books %>% filter(books$ISBN == isbn_no) %>% select(Year)
                    year <- year$Year[1]
                    # RatingInfo
                    avg_rating_final <- Book_rating(isbn_no)
                    #-----------------------------------------
                    
                    #sr = cover_name(data_filtered$URL[(i - 1) * nbooks_per_row + j]) 
                    
                    a(box(width = 2, height = 270,
                          div(style = "text-align:center", img(id = isbn_no,
                                                               src = data_filtered_ubcf$URL[(i - 1) * nbooks_per_row + j], 
                                                               width = 100, height = 150,
                                                               style="cursor:pointer;")),
                          
                          useShinyjs(),
                          shinyjs::onclick(isbn_no,  updateTabsetPanel(session, inputId="inTabset", selected="Item Details")),
                          
                          shinyjs::onclick(isbn_no, book_info(isbn_no), add = TRUE),
                          
                          shinyjs::onclick(isbn_no, arm_recc(isbn_no), add = TRUE),
                          
                          
                          div(style = "text-align:center; color: #999999; font-size: 80%", data_filtered_ubcf$Author[(i - 1) * nbooks_per_row + j]),
                          div(style = "text-align:center", strong(data_filtered_ubcf$Title[(i - 1) * nbooks_per_row + j])),
                          
                          div(style = "text-align:center; color : #f0ad4e",
                              ratingInput(paste0("bookRating_",isbn_no), label="", class = "rating",
                                          # dataFilled="fa fa-star",
                                          # dataEmpty="fa fa-star-o",
                                          dataStart=0,
                                          dataStop=5,
                                          dataStep = 1,
                                          dataFractions=2,
                                          disabled=TRUE,
                                          value= avg_rating_final))))
                  })))
                })
                
              }
            )
            }
            else
            {
              output$UBCF_book_recc_display <- renderUI({
                column(8, offset = 0.5,
                       img(src = "sad_face.jpeg",   # A sad face is displayed when the algorithm cant produce recommendations
                           width = 180, height = 150))
              })
            }
            
            
            
            
            #-------------------------
            # IBCF_recc_items
            #-------------------------
            # Collaborative filtering results(UBCF as well as IBCF) should be displayed only when the user logs in.
            
            ibcf_recc_items <- ibcf(id = input$userid)
            
            # Converting to a normal proper list format
            if(ibcf_recc_items[input$userid] != 'character(0)')
            {
              
            ibcf_recc_items_list <- c()
            for (i in 1:5)
            {
              ibcf_recc_items_list <- c(ibcf_recc_items_list, ibcf_recc_items[[input$userid]][i])
            }
              
              # Filtering the data with only those books
              data_filtered_ibcf <- books %>% filter(books$ISBN %in% ibcf_recc_items_list)
              data_filtered_ibcf <- data_filtered_ibcf[!duplicated(data_filtered_ibcf[,c('Title')]),]
              
              # Displaying the books in Items tab under the corresponding box allocated for it in UI page:
              output$IBCF_book_recc_display <- renderUI(
                {
                  nbooks <- nrow(data_filtered_ibcf)
                  nbooks_per_row <- nrow(data_filtered_ibcf)
                  nrows <- 1
                  
                  lapply(1:nrows, function(i) {
                    list(fluidRow(lapply(1:nbooks_per_row, function(j) {
                      
                      isbn_no <- data_filtered_ibcf$ISBN[(i - 1) * nbooks_per_row + j]
                      #-------------------------------
                      book_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Title)
                      book <- book_df$Title[1]
                      # CoverImg
                      url_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Image.URL.L)
                      url_no <- url_df$Image.URL.L[1]
                      # Author
                      author_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Author)
                      author <- author_df$Author[1]
                      # Genre
                      genre_df <- books %>% filter(books$ISBN == isbn_no) %>% select(Genre)
                      genre <- genre_df$Genre[1]
                      # Year
                      year <- books %>% filter(books$ISBN == isbn_no) %>% select(Year)
                      year <- year$Year[1]
                      # RatingInfo
                      avg_rating_final <- Book_rating(isbn_no)
                      #-----------------------------------------
                      
                      a(box(width = 2, height = 270,
                            div(style = "text-align:center", img(id = isbn_no,
                                                                 src = data_filtered_ibcf$URL[(i - 1) * nbooks_per_row + j], 
                                                                 width = 100, height = 150,
                                                                 style="cursor:pointer;")),
                            
                            useShinyjs(),
                            shinyjs::onclick(isbn_no,  updateTabsetPanel(session, inputId="inTabset", selected="Item Details")),
                            
                            shinyjs::onclick(isbn_no, book_info(isbn_no), add = TRUE),
                            
                            shinyjs::onclick(isbn_no, arm_recc(isbn_no), add = TRUE),
                            
                            div(style = "text-align:center; color: #999999; font-size: 80%", data_filtered_ibcf$Author[(i - 1) * nbooks_per_row + j]),
                            div(style = "text-align:center", strong(data_filtered_ibcf$Title[(i - 1) * nbooks_per_row + j])),
                            
                            div(style = "text-align:center; color : #f0ad4e",
                                ratingInput(paste0("bookRating_",isbn_no), label="", class = "rating",
                                            # dataFilled="fa fa-star",
                                            # dataEmpty="fa fa-star-o",
                                            dataStart=0,
                                            dataStop=5,
                                            dataStep = 1,
                                            dataFractions=2,
                                            disabled=TRUE,
                                            value= avg_rating_final))))
                      
                    })))
                  })
                  
                })
            }
            else
              {
              output$IBCF_book_recc_display <- renderUI({
                column(8, offset = 0.5,
                       img(src = "sad_face.jpeg",    # A sad face is displayed when the algorithm cant produce recommendations
                           width = 180, height = 150))
              })
              }
            
            
            
            
            
          }
          
       })
          


})

 


   