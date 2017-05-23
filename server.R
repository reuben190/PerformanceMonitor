

library(shiny)
library(dygraphs)
library(AnomalyDetection)
library(zoo)

shinyServer(function(input, output,session) {
  #Base Data
  data=reactive({data=DTRData})
  
  #Render BaseData table
  output$table_baseData=DT::renderDataTable(DT::datatable(data()[,.(`Order #`,`Report Upload Date`=ymd(format(`Report Upload Date`,"%Y-%m-%d")) ,State=factor(State),Region=factor(Region),County,`Service Group`=factor(`Service Group`),`Lender`=factor(`Lender Corporate Name`),`Appraiser`,`Market Type`,`Area Type`=factor(`Area Type`),`Region Manager`=factor(`Region Manager`),`Appraisal Purpose`=factor(`Appraisal Purpose`))]
                                                          ,class = 'cell-border stripe',filter = 'top', options = list(
                                                            pageLength = 3,scrollX = TRUE, autoWidth = TRUE,
                                                            ajax = list(
                                                              serverSide = TRUE, processing = TRUE
                                                            )
                                                          ),rownames = FALSE
  ))
  
  data_summary=reactive({
    
    req(input$table_baseData_rows_all)
    if(!is.null(input$table_baseData_rows_all)){
      data=data()[input$table_baseData_rows_all]
    }
    else{
      data=data()
    }
    data=data[is.na(`Cancel Date`) & !is.na(`Complete Date`),
              .(`Units`=length(`Order ID`),
                `Lender Fee`=round(mean(`Final Invoice Amount`,na.rm=T),2),
                `Appr Fee`=round(mean(`Appraiser's Fee`,na.rm=T),2),
                `Margin`=round(mean(`Management Fee`,na.rm=T),2),
                Performance=round(mean(Performance,na.rm = T),2),
                InspectTAT=round(mean(InspectTAT,na.rm=T),2),
                UploadTAT=round(mean(UploadTAT,na.rm=T),2),
                `1stDeliveryTAT`=round(mean(`1stDeliveryTAT`,na.rm=T),2),
                Quality=round(mean( Quality,na.rm = T),2)
              ),by=eval(c("Months",names(GroupBy_Actual[unlist(GroupBy_Actual%in% as.numeric(input$group))])))]  })
  output$table_baseDataSummary=DT::renderDataTable(DT::datatable(data_summary(), class = 'cell-border stripe', rownames = FALSE,options = list(pageLength = 5,scrollX = TRUE, autoWidth = TRUE),
                                                                 colnames = c("Appraiser Fee"=5,"TAT (Inspect)"=8,"TAT (Upload)"=9,"TAT (First Deliver)"=10)) )

  ###Units
  data_TS_Units=reactive({
    require(input$group)
    Units=dcast(data_summary(),formula = paste0('Months~','`',names(GroupBy_Actual[unlist(GroupBy_Actual%in% as.numeric(input$group))]),'`'),value.var = c("Units"))
    ts(Units,start = c(2015,1),frequency = 12)

    })
  output$data_units=renderUI({
    selectInput(inputId = "units",label = "Select Elements",multiple = T,choices = dimnames(data_TS_Units())[[2]][-1],selected=dimnames(data_TS_Units())[[2]][-1][10:11])
  })
   output$dygraph_units <- renderDygraph({

    if(length(na.omit(input$units))>1){
       Events=apply(data_TS_Units()[,input$units],MARGIN = 2,function(X){AnomalyDetectionVec(X, max_anoms=0.2, direction="both", plot=TRUE,period = 12)$anoms})
       Events=rbindlist(Events)
       Events=Events[!duplicated(Events$index)]
       dygraph( data_TS_Units()[,input$units])%>%
       dyOptions(drawPoints = TRUE, pointSize = 2)%>%dyEvent(x = as.yearmon(time(data_TS_Units()))[Events$index],label = rep("Investigate",length(Events$index)),color = "red", labelLoc = "bottom")
       
    }
     else{
        Events=AnomalyDetectionVec(data_TS_Units()[,input$units], max_anoms=0.2, direction="both", plot=TRUE,period = 12)$anoms
        Events=Events[!duplicated(Events$index)]
         dygraph( data_TS_Units()[,input$units])%>%dyOptions(drawPoints = TRUE, pointSize = 2)%>%dyEvent(x = as.yearmon(time(data_TS_Units()))[Events$index],label = rep("Investigate",length(Events$index)),color = "red", labelLoc = "bottom")
     }
     
     
   })
   
   ###LenderFee
   data_TS_LenderFee=reactive({
     require(input$group)
     Units=dcast(data_summary(),formula = paste0('Months~','`',names(GroupBy_Actual[unlist(GroupBy_Actual%in% as.numeric(input$group))]),'`'),value.var = c("Lender Fee"))
     ts(Units,start = c(2015,1),frequency = 12)
     
   })
   output$data_LenderFee=renderUI({
     selectInput(inputId = "lenderfee",label = "Select Elements",multiple = T,choices = dimnames(data_TS_Units())[[2]][-1],selected = dimnames(data_TS_Units())[[2]][-1][1])
   })
   output$dygraph_lenderfee <- renderDygraph({
     dygraph(data_TS_LenderFee()[,input$lenderfee])%>%
       dyOptions(drawPoints = TRUE, pointSize = 2)
   })
   
   ###Appraiser Fee
   data_TS_AppraiserFee=reactive({
     require(input$group)
     Units=dcast(data_summary(),formula = paste0('Months~','`',names(GroupBy_Actual[unlist(GroupBy_Actual%in% as.numeric(input$group))]),'`'),value.var = c("Appr Fee"))
     ts(Units,start = c(2015,1),frequency = 12)
     
   })
   output$data_AppraiserFee=renderUI({
     selectInput(inputId = "appraiserfee",label = "Select Elements",multiple = T,choices = dimnames(data_TS_AppraiserFee())[[2]][-1],selected = dimnames(data_TS_Units())[[2]][-1][1])
   })
   output$dygraph_appraiserfee <- renderDygraph({
     dygraph(data_TS_AppraiserFee()[,input$lenderfee])%>%
       dyOptions(drawPoints = TRUE, pointSize = 2)
   })
   
   ###margin
   data_TS_Margin=reactive({
     require(input$group)
     Units=dcast(data_summary(),formula = paste0('Months~','`',names(GroupBy_Actual[unlist(GroupBy_Actual%in% as.numeric(input$group))]),'`'),value.var = c("Margin"))
     ts(Units,start = c(2015,1),frequency = 12)
     
   })
   output$data_Margin=renderUI({
     selectInput(inputId = "margin",label = "Select Elements",multiple = T,choices = dimnames(data_TS_Margin())[[2]][-1],selected = dimnames(data_TS_Units())[[2]][-1][1])
   })
   output$dygraph_margin <- renderDygraph({
     dygraph(data_TS_Margin()[,input$margin])%>%
       dyOptions(drawPoints = TRUE, pointSize = 2)
   })
   
   ###Perfromance
   data_TS_Performance=reactive({
     require(input$group)
     Units=dcast(data_summary(),formula = paste0('Months~','`',names(GroupBy_Actual[unlist(GroupBy_Actual%in% as.numeric(input$group))]),'`'),value.var = c("Performance"))
     ts(Units,start = c(2015,1),frequency = 12)
     
   })
   output$data_Performance=renderUI({
     selectInput(inputId = "performace",label = "Select Elements",multiple = T,choices = dimnames(data_TS_Performance())[[2]][-1],selected = dimnames(data_TS_Performance())[[2]][-1][1])
   })
   output$dygraph_performance <- renderDygraph({
     dygraph(data_TS_Performance()[,input$margin])%>%
       dyOptions(drawPoints = TRUE, pointSize = 2)
   })
   
   ###Perfromance
   data_TS_InspectTAT=reactive({
     require(input$group)
     Units=dcast(data_summary(),formula = paste0('Months~','`',names(GroupBy_Actual[unlist(GroupBy_Actual%in% as.numeric(input$group))]),'`'),value.var = c("InspectTAT"))
     ts(Units,start = c(2015,1),frequency = 12)
     
   })
   output$data_InspectTAT=renderUI({
     selectInput(inputId = "InspectTAT",label = "Select Elements",multiple = T,choices = dimnames(data_TS_InspectTAT())[[2]][-1],selected = dimnames(data_TS_InspectTAT())[[2]][-1][1])
   })
   output$dygraph_InspectTAT <- renderDygraph({
     dygraph(data_TS_InspectTAT()[,input$InspectTAT])%>%
       dyOptions(drawPoints = TRUE, pointSize = 2)
   })
   
   ###Perfromance
   data_TS_UploadTAT=reactive({
     require(input$group)
     Units=dcast(data_summary(),formula = paste0('Months~','`',names(GroupBy_Actual[unlist(GroupBy_Actual%in% as.numeric(input$group))]),'`'),value.var = c("UploadTAT"))
     ts(Units,start = c(2015,1),frequency = 12)
     
   })
   output$data_UploadTAT=renderUI({
     selectInput(inputId = "UploadTAT",label = "Select Elements",multiple = T,choices = dimnames(data_TS_UploadTAT())[[2]][-1],selected = dimnames(data_TS_UploadTAT())[[2]][-1][1])
   })
   output$dygraph_UploadTAT <- renderDygraph({
     dygraph(data_TS_UploadTAT()[,input$UploadTAT])%>%
       dyOptions(drawPoints = TRUE, pointSize = 2)
   })
   
   ###Perfromance
   data_TS_1stDeliveryTAT=reactive({
     require(input$group)
     Units=dcast(data_summary(),formula = paste0('Months~','`',names(GroupBy_Actual[unlist(GroupBy_Actual%in% as.numeric(input$group))]),'`'),value.var = c("1stDeliveryTAT"))
     ts(Units,start = c(2015,1),frequency = 12)
     
   })
   output$data_1stDeliveryTAT=renderUI({
     selectInput(inputId = "1stDeliveryTAT",label = "Select Elements",multiple = T,choices = dimnames(data_TS_1stDeliveryTAT())[[2]][-1],selected = dimnames(data_TS_1stDeliveryTAT())[[2]][-1][1])
   })
   output$dygraph_1stDeliveryTAT<- renderDygraph({
     dygraph(data_TS_1stDeliveryTAT()[,input$`1stDeliveryTAT`])%>%
       dyOptions(drawPoints = TRUE, pointSize = 2)
   })
   
   ###Perfromance
   data_TS_Quality=reactive({
     require(input$group)
     Units=dcast(data_summary(),formula = paste0('Months~','`',names(GroupBy_Actual[unlist(GroupBy_Actual%in% as.numeric(input$group))]),'`'),value.var = c("Quality"))
     ts(Units,start = c(2015,1),frequency = 12)
     
   })
   output$data_Quality=renderUI({
     selectInput(inputId = "Quality",label = "Select Elements",multiple = T,choices = dimnames(data_TS_Quality())[[2]][-1],selected = dimnames(data_TS_Quality())[[2]][-1][1])
   })
   output$dygraph_Quality<- renderDygraph({
     dygraph(data_TS_Quality()[,input$`Quality`])%>%
       dyOptions(drawPoints = TRUE, pointSize = 2)
   })
  })
