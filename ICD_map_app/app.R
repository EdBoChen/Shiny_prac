#load chunk
icd_test<- readRDS("Shint_prac/ICD_map.RData")

#library pkgs
pkg<- c("shiny", "shinytreeview", "shinyWidgets", "knitr", "kableExtra", "magrittr", "stringr", "dplyr")
lapply(pkg, require, character.only= T)

#Define the order of the tree
treeorder<- unique(rbind(icd_test$icd9[,1:4],icd_test$icd10[,1:4]))%>%
  arrange(cause_lv4, cause_lv3, cause_lv2, cause_lv1)

####
#UI
ui <- fluidPage(
  #selection tree
  column(3,
         tags$h3("ICD code for YLL cause outline"),
         treecheckInput(
           inputId = "tree",
           label = "Cause outline:",
           choices = make_tree(
             treeorder, c("cause_lv1", "cause_lv2", "cause_lv3", "cause_lv4")
           ),
           width = "100%"
         )),
  column(8,
         tabsetPanel(type = "tabs",
                     #total list
                     tabPanel("Maps to NBD cause",
                              column(12, downloadButton("downloadData", "Download"), offset= 12),
                              column(6, tags$h4("ICD-9 code for selected cause outline"),
                                     tableOutput("icd9_list")),
                              column(6, tags$h4("ICD-10 code for selected cause outline"),
                                     tableOutput("icd10_list"))),
                     #comparison tbl
                     tabPanel("Comparison",
                              column(6, tags$h4("ICD-9 code for selected cause outline"),
                                     tags$h6("Same ICD descriptions"),
                                     tableOutput("icd9_same_tbl"),
                                     fixedRow(
                                       column(12,
                                              tags$h6("Different ICD descriptions"),
                                              tableOutput("icd9_diff_tbl"))
                                     )
                              ),
                              column(6, tags$h4("ICD-10 code for selected cause outline"),
                                     tags$h6("Same ICD descriptions"),
                                     tableOutput("icd10_same_tbl"),
                                     fixedRow(
                                       column(12,
                                              tags$h6("Different ICD descriptions"),
                                              tableOutput("icd10_diff_tbl"))
                                     ))),
                     #inquiry
                     tabPanel("Input searching (only for ICD code)",
                              br(),
                              column(12,
                                     searchInput(inputId= "TextSearch",
                                                 placeholder= "e.g., 434.91 (Cerebral artery occlusion, unspecified with cerebral infarction)",
                                                 btnSearch= icon("search"),
                                                 btnReset= icon("remove"),
                                                 width= "100%"),
                                     fixedRow("ICD 9 codes", tableOutput("icdtext_tbl_icd9"),
                                              br(),
                                              "ICD 10 codes", tableOutput("icdtext_tbl_icd10"))
                              )
                     )
         )))
####
#Server
server <- function(input, output) {
  #Tab1: ICD Maps
  output$icd9_list<- function(){
    req(input$tree)
    icd_test$icd9[icd_test$icd9$cause_lv4%in%input$tree, c("icd_code", "icd_name", "yll_cause_name", "yll_cause", "cause_outline")]%>%
      knitr::kable("html")%>%
      kable_styling("striped", full_width= F)%>%
      scroll_box(width = "100%", height = "960px")
  }
  output$icd10_list<- function(){
    req(input$tree)
    icd_test$icd10[icd_test$icd10$cause_lv4%in%input$tree, c("icd_code", "icd_name", "yll_cause_name", "yll_cause", "cause_outline")]%>%
      knitr::kable("html")%>%
      kable_styling("striped", full_width= F)%>%
      scroll_box(width = "100%", height = "960px")
  }
  #Download buttom
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("ICD_map_to_cause.csv")
    },
    content= function(file){
      write.csv(
        rbind(
          icd_test$icd9[icd_test$icd9$cause_lv4%in%input$tree, c("icd_code", "icd_name", "yll_cause_name", "yll_cause", "cause_outline")]%>%mutate(icd_version= "ICD-9"),
          icd_test$icd10[icd_test$icd10$cause_lv4%in%input$tree, c("icd_code", "icd_name", "yll_cause_name", "yll_cause", "cause_outline")]%>%mutate(icd_version= "ICD-10")),
        file, row.names= FALSE)
    }
  )
  #Tab2: Comparison
  #description tbl
  output$icd9_same_tbl<- function(){
    req(input$tree)
    ext_tbl_r= icd_test$icd9[icd_test$icd9$cause_lv4%in%input$tree,]
    ext_index_r= icd_test$icd9[icd_test$icd9$cause_lv4%in%input$tree,][["icd_name"]]%in%
      icd_test$icd10[icd_test$icd10$cause_lv4%in%input$tree,][["icd_name"]]
    ext_tbl_r[ext_index_r, c("icd_code", "icd_name", "yll_cause_name", "yll_cause", "cause_outline")]%>%
      knitr::kable("html")%>%
      kable_styling("striped", full_width= F)%>%
      scroll_box(width = "100%", height = "360px")
  }
  output$icd10_same_tbl<- function(){
    req(input$tree)
    ext_tbl_l= icd_test$icd10[icd_test$icd10$cause_lv4%in%input$tree,]
    ext_index_l= icd_test$icd10[icd_test$icd10$cause_lv4%in%input$tree,][["icd_name"]]%in%
      icd_test$icd9[icd_test$icd9$cause_lv4%in%input$tree,][["icd_name"]]
    ext_tbl_l[ext_index_l, c("icd_code", "icd_name", "yll_cause_name", "yll_cause", "cause_outline")]%>%
      knitr::kable("html")%>%
      kable_styling("striped", full_width= F)%>%
      scroll_box(width = "100%", height = "360px")
  }
  output$icd9_diff_tbl<- function(){
    req(input$tree)
    ext_tbl_r= icd_test$icd9[icd_test$icd9$cause_lv4%in%input$tree,]
    ext_index_r= icd_test$icd9[icd_test$icd9$cause_lv4%in%input$tree,][["icd_name"]]%in%
      icd_test$icd10[icd_test$icd10$cause_lv4%in%input$tree,][["icd_name"]]
    ext_tbl_r[!ext_index_r, c("icd_code", "icd_name", "yll_cause_name", "yll_cause", "cause_outline")]%>%
      knitr::kable("html")%>%
      kable_styling("striped", full_width= F)%>%
      scroll_box(width = "100%", height = "600px")
  }
  output$icd10_diff_tbl<- function(){
    req(input$tree)
    ext_tbl_l= icd_test$icd10[icd_test$icd10$cause_lv4%in%input$tree,]
    ext_index_l= icd_test$icd10[icd_test$icd10$cause_lv4%in%input$tree,][["icd_name"]]%in%
      icd_test$icd9[icd_test$icd9$cause_lv4%in%input$tree,][["icd_name"]]
    ext_tbl_l[!ext_index_l, c("icd_code", "icd_name", "yll_cause_name", "yll_cause", "cause_outline")]%>%
      knitr::kable("html")%>%
      kable_styling("striped", full_width= F)%>%
      scroll_box(width = "100%", height = "600px")
  }
  #Tab3: Text search
  output$icdtext_tbl_icd9<- function(){
    stext<- gsub("[[:punct:]]", "", input$TextSearch)%>%gsub(pattern= "[[:blank:]]",replacement= "")
    if(sum(str_detect(icd_test$icd9$icd_code, stext))>0){
      icd_test$icd9[which(str_detect(icd_test$icd9$icd_code, stext)), c("icd_code", "icd_name", "yll_cause_name", "yll_cause", "cause_outline")]%>%
        knitr::kable("html")%>%
        kable_styling("striped", full_width= F)%>%
        scroll_box(width = "100%", height = "640px")
    }else{paste0("No matching search results for ICD 10 code found")}
  }
  output$icdtext_tbl_icd10<- function(){
    stext<- gsub("[[:punct:]]", "", input$TextSearch)%>%gsub(pattern= "[[:blank:]]",replacement= "")
    if(sum(str_detect(icd_test$icd10$icd_code, stext))>0){
      icd_test$icd10[which(str_detect(icd_test$icd10$icd_code, stext)), c("icd_code", "icd_name", "yll_cause_name", "yll_cause", "cause_outline")]%>%
        knitr::kable("html")%>%
        kable_styling("striped", full_width= F)%>%
        scroll_box(width = "100%", height = "640px")
    }else{paste0("No matching search results for ICD 10 code found")}
  }
}

if (interactive())
  shinyApp(ui, server)
