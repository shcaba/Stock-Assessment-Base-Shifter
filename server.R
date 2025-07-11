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

# Define server logic required to draw a histogram
function(input, output, session) {
  spp.out<-SS_output(paste0(getwd(),"/Spp_Reports/REBS_2025"))
  observe({

  Spp.dervout <- data.frame(Year=spp.out$timeseries$Yr,TotalB=spp.out$timeseries$Bio_all,SummaryB=spp.out$timeseries$Bio_smry,SpawnOut<-spp.out$timeseries$SpawnBio,Dep<-spp.out$timeseries$SpawnBio/spp.out$timeseries$SpawnBio[1])
  

    if(!any(spp.out$timeseries$Yr==input$Year_comp))
    {
      Spp.dervout.gg <- rbind(data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_all/spp.out$timeseries$Bio_all[1],Metric="Total Biomass"),
                              data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_smry/spp.out$timeseries$Bio_smry[1],Metric="Summary Biomass"),
                              data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$SpawnBio/spp.out$timeseries$SpawnBio[1],Metric="Spawning Output"))
    }
    
    if(any(spp.out$timeseries$Yr==input$Year_comp))
    {
      Spp.dervout.gg <- rbind(data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_all/spp.out$timeseries$Bio_all[spp.out$timeseries$Yr==input$Year_comp],Metric="Total Biomass"),
                              data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_smry/spp.out$timeseries$Bio_smry[spp.out$timeseries$Yr==input$Year_comp],Metric="Summary Biomass"),
                              data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$SpawnBio/spp.out$timeseries$SpawnBio[spp.out$timeseries$Yr==input$Year_comp],Metric="Spawning Output"))
    }
    output$CompPlot <- renderPlotly({
      comp1<-ggplot(Spp.dervout.gg,aes(Year,Value,col=Metric))+
        geom_line(lwd=1.25)+
        ylab("Value relative to chosen year")+
        ylim(0,NA)+
        geom_hline(yintercept=1,col="orange",linetype="dashed")+
        geom_vline(xintercept=input$Year_comp,col="orange",linetype="dashed")+
        theme_bw()
      
      ggplotly(comp1)
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
