shinyServer(function(input, output) {
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
)
