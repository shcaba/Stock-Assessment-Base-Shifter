#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(r4ss)
library(ggplot2)
library(plotly)
library(shinyFiles)
library(shinybusy)
library(wesanderson)

# Define server logic required to draw a histogram
function(input, output, session) {

  volumes <- getVolumes()()
  
  pathReport <- reactive({
    shinyDirChoose(input,"Report_dir",roots=volumes,session=session,filetypes=c('', 'txt'))
    return(parseDirPath(volumes, input$Report_dir))
  })
  
  model.out <- reactiveValues(report = NULL)
    # if(!is.null(input$Report_dir))
#  {
#    
  
  observeEvent(input$Report_dir,{
  output$ReportPath <- renderText({paste0("Selected model folder:\n", pathReport())})
  })
  
  #spp.out<-SS_output(paste0(getwd(),"/Spp_Reports/REBS_2025"))
  
  observeEvent(req(pathReport()),{
    show_modal_spinner(spin="flower",color=wes_palettes$AsteroidCity1[1],text="Reading in model output")
    model.out$report<-SS_output(pathReport())
    remove_modal_spinner()
  })
  
  observeEvent(input$run_baseline_comps,{
    spp.out<-model.out$report
    Spp.dervout <- data.frame(Year=spp.out$timeseries$Yr,TotalB=spp.out$timeseries$Bio_all,SummaryB=spp.out$timeseries$Bio_smry,SpawnOut<-spp.out$timeseries$SpawnBio,Dep<-spp.out$timeseries$SpawnBio/spp.out$timeseries$SpawnBio[1])
    Spp.dervout.RE <- data.frame(Year=spp.out$timeseries$Yr,TotalB.RE=(spp.out$timeseries$Bio_all-spp.out$timeseries$Bio_all[1])/spp.out$timeseries$Bio_all[1],SummaryB=(spp.out$timeseries$Bio_smry-spp.out$timeseries$Bio_smry[1])/spp.out$timeseries$Bio_smry[1],SpawnOut.RE<-(spp.out$timeseries$SpawnBio-spp.out$timeseries$SpawnBio[1])/spp.out$timeseries$SpawnBio[1])

        if(!any(spp.out$timeseries$Yr==input$Year_comp))
  {
    Spp.dervout.gg <- rbind(data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_all/spp.out$timeseries$Bio_all[1],Metric="Total Biomass"),
                            data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_smry/spp.out$timeseries$Bio_smry[1],Metric="Summary Biomass"),
                            data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$SpawnBio/spp.out$timeseries$SpawnBio[1],Metric="Spawning Output"))
    
    Spp.dervout.RE.gg <- rbind(data.frame(Year=spp.out$timeseries$Yr,Value=(spp.out$timeseries$Bio_all-spp.out$timeseries$Bio_all[1])/spp.out$timeseries$Bio_all[1],Metric="Total Biomass"),
                               data.frame(Year=spp.out$timeseries$Yr,Value=(spp.out$timeseries$Bio_smry-spp.out$timeseries$Bio_smry[1])/spp.out$timeseries$Bio_smry[1],Metric="Summary Biomass"),
                               data.frame(Year=spp.out$timeseries$Yr,Value=(spp.out$timeseries$SpawnBio-spp.out$timeseries$SpawnBio[1])/spp.out$timeseries$SpawnBio[1],Metric="Spawning Output"))
    
  }
  
  if(any(spp.out$timeseries$Yr==input$Year_comp))
  {
    Spp.dervout.gg <- rbind(data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_all/spp.out$timeseries$Bio_all[spp.out$timeseries$Yr==input$Year_comp],Metric="Total Biomass"),
                            data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_smry/spp.out$timeseries$Bio_smry[spp.out$timeseries$Yr==input$Year_comp],Metric="Summary Biomass"),
                            data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$SpawnBio/spp.out$timeseries$SpawnBio[spp.out$timeseries$Yr==input$Year_comp],Metric="Spawning Output"))
    
    Spp.dervout.RE.gg <- rbind(data.frame(Year=spp.out$timeseries$Yr,Value=(spp.out$timeseries$Bio_all-spp.out$timeseries$Bio_all[spp.out$timeseries$Yr==input$Year_comp])/spp.out$timeseries$Bio_all[spp.out$timeseries$Yr==input$Year_comp],Metric="Total Biomass"),
                               data.frame(Year=spp.out$timeseries$Yr,Value=(spp.out$timeseries$Bio_smry-spp.out$timeseries$Bio_smry[spp.out$timeseries$Yr==input$Year_comp])/spp.out$timeseries$Bio_smry[spp.out$timeseries$Yr==input$Year_comp],Metric="Summary Biomass"),
                               data.frame(Year=spp.out$timeseries$Yr,Value=(spp.out$timeseries$SpawnBio-spp.out$timeseries$SpawnBio[spp.out$timeseries$Yr==input$Year_comp])/spp.out$timeseries$SpawnBio[spp.out$timeseries$Yr==input$Year_comp],Metric="Spawning Output"))
  }

  output$CompPlot <- renderPlotly({
    comp1<-ggplot(Spp.dervout.gg,aes(Year,Value,col=Metric))+
      geom_line(lwd=1.25)+
      ylab("Value relative to chosen year")+
      ylim(0,NA)+
      geom_hline(yintercept=1,col="orange",linetype="dashed")+
      geom_vline(xintercept=input$Year_comp,col="orange",linetype="dashed")+
      theme_bw() #+
      #annotate("rect",xmin=-Inf, xmax=input$Year_comp, ymin=-Inf, ymax=Inf, alpha=0.75, fill='darkgray')
    
     comp1<-ggplotly(comp1)

     comp1[['x']][['layout']][['shapes']] <- c()
     comp1<- layout(comp1,
               shapes = list(
                 list(type = "rect",
                      fillcolor = "gray", line = list(color = "gray"), opacity = 0.25,
                      x0 = 0, x1 = input$Year_comp,
                      y0 = 0, y1 = 1000
                 )
               ))
     comp1
  })
  
  output$CompPlotRE <- renderPlotly({
    comp2<-ggplot(Spp.dervout.RE.gg,aes(Year,Value*100,col=Metric))+
      geom_segment(aes(x=Year,xend=Year,y=0,yend=Value))+
      geom_point(size=4) +
      ylab("Percent difference from chosen year")+
      geom_hline(yintercept=0,col="orange",linetype="dashed")+
      geom_vline(xintercept=input$Year_comp,col="orange",linetype="dashed")+
      theme_bw() #+
      #annotate("rect",xmin=-Inf, xmax=input$Year_comp, ymin=-Inf, ymax=Inf, alpha=0.5, fill='gray')
    
      comp2<-ggplotly(comp2)
      
      comp2[['x']][['layout']][['shapes']] <- c()
      comp2<- layout(comp2,
                     shapes = list(
                       list(type = "rect",
                            fillcolor = "gray", line = list(color = "gray"), opacity = 0.25,
                            x0 = 0, x1 = input$Year_comp,
                            y0 = -1000, y1 = 1000
                       )
                     ))
      comp2
  })
  
  output$DepPlot <- renderPlotly({
    p1<-ggplot(Spp.dervout,aes(Year,Dep))+
      geom_line(lwd=1.25)+
      ylab("Relative Stock Status")+
      ylim(0,NA)+
      theme_bw()
    ggplotly(p1)
  })
  
  output$SpawnOutPlot <- renderPlotly({
    p2<-ggplot(Spp.dervout,aes(Year,SpawnOut))+
      geom_line(lwd=1.25)+
      ylab("Spawning Output")+
      ylim(0,NA)+
      theme_bw()
    ggplotly(p2)
  })
  
  output$SummaryBPlot <- renderPlotly({
    p3<-ggplot(Spp.dervout,aes(Year,SummaryB))+
      geom_line(lwd=1.25)+
      ylab("Summary Biomass")+
      ylim(0,NA)+
      theme_bw()
    ggplotly(p3)
  })
  
  
  output$TotalBPlot <- renderPlotly({
    p4<-ggplot(Spp.dervout,aes(Year,TotalB))+
      geom_line(lwd=1.25)+
      ylab("Total Biomass")+
      ylim(0,NA)+
      theme_bw()
    ggplotly(p4)
  })
  
  })    
}
