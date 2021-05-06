#load chunk
icd_test<- readRDS("D:/Ed_TBDC/R_proj/interactive_interface/ICD_map.RData")

#library pkgs
pkg<- c("shiny", "shinytreeview", "shinyWidgets", "knitr", "kableExtra", "magrittr", "stringr", "dplyr")
lapply(pkg, require, character.only= T)

#Define the order of the tree
treeorder<- unique(rbind(icd_test$icd9[,1:4],icd_test$icd10[,1:4]))%>%
  arrange(cause_lv4, cause_lv3, cause_lv2, cause_lv1)

####
#UI
shinyUI(fluidPage(
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
)
