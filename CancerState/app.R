#Viz the difference between stages
#load
library(shiny)
library(magrittr)
library(data.table)
library(dplyr)
library(shinyauthr)
library(remotes)
library(htmltools)

cancer_df<- fread("/Users/liupochen_macbook/Desktop/TBDC/Zoey/cancer_p.csv")

user_base <- tibble::tibble(
  user = c("tbdc"),
  password = c("tbdc752"),
  permissions = c("admin"),
  name = c("TBDC")
)

#ui
ui<- fluidPage(
  shinyauthr::loginUI(id = "login"),
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  uiOutput("mainUI")
)

#server
server <- function(input, output) {
  logout_init <- callModule(
    shinyauthr::logout,
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  credentials <- callModule(
    shinyauthr::login,
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  user_data <- reactive({
    credentials()$info
  })
  output$mainUI<- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      titlePanel(title=div(img(src = "TBDC_logo_ed.jpeg", height = 80, width = 80),
                           span("The Checking Tool of Cancer State", 
                                style = "color: gray; font-size: 48px"))),
      #function ui
      column(12, 
             sliderInput("yrSel", "Year slider",
                         min= min(2000),
                         max= max(2015),
                         value= min(2000),
                         step= 1,
                         ticks= F,
                         animate= animationOptions(interval= 840, loop= FALSE),
                         sep= "",
                         width= 480
                        ),
             align= "center"
             ),
      column(12,
             fluidRow(
               column(12, highchartOutput("state_prop", height= 720)),
               column(12, highchartOutput("state_count", height= 720))
             )
             ),
      
      )
  })
  
  #server define
  output$state_prop<- renderHighchart({
    req(credentials()$user_auth)
    req(input$yrSel)
    
    tmp<- cancer_df[cancer_df$YEAR==input$yrSel,]%>%
      group_by(sequelae, csite, YEAR)%>%
      summarise(prev_n= sum(prevalent_case_number))%>%ungroup
    cancer_sum<- tmp%>%group_by(csite, YEAR)%>%summarise(p_sum= sum(prev_n))%>%ungroup
    tmp%<>%left_join(cancer_sum, by= c("YEAR", "csite"))%>%
      mutate(prev_prop= prev_n/p_sum)
    
    hchart(tmp,
           'column', hcaes(x = 'csite', y = 'prev_prop', group = 'sequelae'),
           stacking = "normal")%>%
      hc_yAxis(title= list(text= "Proportion of states"))%>%
      hc_xAxis(title= list(text= "Sites of cancers"))%>%
      hc_legend(title= list(text= "States"))%>%
      hc_title(text= "The comparison of the proportion between cancer states")%>%
      hc_subtitle(text= paste0(input$yrSel))%>%
      hc_plotOptions(
        series= list(animation= list(duration=0)),
        drilldown= list(animation= list(duration=0))
      )
  })
  
  output$state_count<- renderHighchart({
    req(credentials()$user_auth)
    req(input$yrSel)
    
    tmp<- cancer_df[cancer_df$YEAR==input$yrSel,]%>%
      group_by(sequelae, csite, YEAR)%>%
      summarise(prev_n= sum(prevalent_case_number))%>%ungroup
    
    hchart(tmp,
           'column', hcaes(x = 'csite', y = 'prev_n', group = 'sequelae'),
           stacking = "normal")%>%
      hc_yAxis(title= list(text= "The number prevalence between cancer states"))%>%
      hc_xAxis(title= list(text= "Sites of cancers"))%>%
      hc_legend(title= list(text= "States"))%>%
      hc_title(text= "The comparison of the prevalence between cancer states")%>%
      hc_subtitle(text= paste0(input$yrSel))%>%
      hc_plotOptions(
        series= list(animation= list(duration=0)),
        drilldown= list(animation= list(duration=0))
      )
  })
}

shinyApp(ui, server)
