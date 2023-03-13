$(function() {

    // Enables linking to specific tabs:
    if (window.location.hash){
      var hash = $.trim(window.location.hash);
      var tab = decodeURI(hash.substring(1, 100));
      $('a[data-value=\"'+tab+'\"]').click();
    }
    // Usage: append the tabName to the URL after the hash.

    // Reveals the KPI dropdown menu at launch:
    $('ul.sidebar-menu li.treeview').first().addClass('active');

    // Update the URL in the browser when a tab is clicked on:
    $('a[href^=#shiny-tab]').click(function(){
      window.location.hash = encodeURI($(this).attr('data-value'));
    })

});

PASSWORD <- data.frame(
  Brukernavn = c("ray","gil"), 
  Passord = c("0000","1234")
  )

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      textInput("userName", "User Name:"),
      passwordInput("passwd", "Pass word:"),
      br(),
      actionButton("Login", "Log in")
    )
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    USER$pass
  }  
})

# Login info during session ----
output$userPanel <- renderUI({
  if (USER$Logged == TRUE) {
    fluidRow(
      column(2,
             "User: ", USER$name
      ),
      column(1, actionLink("logout", "Logout"))
    )
  }  
})

# control login
observeEvent(input$Login , {
  Username <- isolate(input$userName)
  Password <- isolate(input$passwd)
  Id.username <- which(PASSWORD$Brukernavn == Username)
  Id.password <- which(PASSWORD$Passord    == Password)
  if (length(Id.username) > 0 & length(Id.password) > 0) {
    if (Id.username == Id.password) {
      USER$Logged <- TRUE
      USER$name <- Username      
    } 
  } else {
    USER$pass <- "User name or password failed!"
  }
})

# control logout
observeEvent(input$logout , {
  USER$Logged <- FALSE
  USER$pass <- ""
})