server <- function(input, output) {
  

observeEvent(input$radio_button,{
  
  if(input$radio_button == "Nessuno"){
    hide("hist1")
    hide("hist2")
    hide("hist3")
    hide("hist4")
    hide("boxplot_plot1")
    hide("boxplot_plot2")
    hide("boxplot_plot3")
    hide("boxplot_plot4")
    hide("qq_plot1")
    hide("qq_plot2")
    hide("qq_plot3")
    hide("qq_plot4")
    hide("corr1")
    hide("corr2")
    hide("vid_tend")
    hide("piu_views")
    hide("piu_likes")
    hide("piu_dislikes")
  }
  
  if(input$radio_button == "Istogrammi"){
    show("hist1")
    show("hist2")
    show("hist3")
    show("hist4")
    
    hide("boxplot_plot1")
    hide("boxplot_plot2")
    hide("boxplot_plot3")
    hide("boxplot_plot4")
    hide("qq_plot1")
    hide("qq_plot2")
    hide("qq_plot3")
    hide("qq_plot4")
    hide("corr1")
    hide("corr2")
    hide("vid_tend")
    hide("piu_views")
    hide("piu_likes")
    hide("piu_dislikes")
    
  
    output$hist1 <- renderPlot ({
      hist(DFvideo$views, breaks = 1000, xlim = c(0, 8000000), ylim = c(0, 8000),
         xlab = "Numero di views per video", main = "Istogramma delle views", col = "steelblue")
      abline(v = views_mean, type = "l", col = "yellow", lwd = 2)
      abline(v=views_q[1], col="red", lwd=2) # 0% (min)
      abline(v=views_q[2], col="blue", lwd=2) # 1st quartile 25%
      abline(v=views_q[3], col="green", lwd=2.5) # Median value distribution 50%
      abline(v=views_q[4], col="blue", lwd=2) # 3rd quartile 75%
      abline(v=views_q[5], col="red", lwd=2) # 100% (max)
      
      legend("topright", legend = c("media", "Minimo (0%)", "Primo quantile (25%)", "Mediana (50%)", "Terzo quantile (75%)", "Massimo (100%)"), 
             lwd =2, col = c("yellow", "red", "blue", "green", "blue", "red"))
  })
  
   output$hist2 <- renderPlot ({
    hist(DFvideo$likes, xlim = c(0, 450000), breaks = 1000, ylim = c(0, 20000), 
         xlab = "Numero di likes per video", main = "Istogramma dei likes", col = "pink")
     abline(v = likes_mean, type = "l", col = "yellow", lwd = 2)
     abline(v=likes_q[1], col="red", lwd=2) # 0% (min)
     abline(v=likes_q[2], col="blue", lwd=2) # 1st quartile 25%
     abline(v=likes_q[3], col="green", lwd=2.5) # Median value distribution 50%
     abline(v=likes_q[4], col="blue", lwd=2) # 3rd quartile 75%
     abline(v=likes_q[5], col="red", lwd=2) # 100% (max)
     legend("topright", legend = c("media", "Minimo (0%)", "Primo quantile (25%)", "Mediana (50%)", "Terzo quantile (75%)", "Massimo (100%)"), 
            lwd =2, col = c("yellow", "red", "blue", "green", "blue", "red"))
     
  })
  
    output$hist3 <- renderPlot ({
    hist(DFvideo$dislikes, xlim = c(0, 18000), breaks = 1000, ylim = c(0, 40000),
         xlab = "Numero di dislikes per video", main = "Istogramma dei dislikes", col = "gray")
      abline(v = dislikes_mean, type = "l", col = "yellow", lwd = 2)
      abline(v=dis_q[1], col="red", lwd=2) # 0% (min)
      abline(v=dis_q[2], col="blue", lwd=2) # 1st quartile 25%
      abline(v=dis_q[3], col="green", lwd=2.5) # Median value distribution 50%
      abline(v=dis_q[4], col="blue", lwd=2) # 3rd quartile 75%
      abline(v=dis_q[5], col="red", lwd=2) # 100% (max)
      legend("topright", legend = c("media", "Minimo (0%)", "Primo quantile (25%)", "Mediana (50%)", "Terzo quantile (75%)", "Massimo (100%)"), 
             lwd =2, col = c("yellow", "red", "blue", "green", "blue", "red"))
  })
  
   output$hist4 <- renderPlot ({
    hist(DFvideo$comment_count, xlim = c(0, 28000), breaks = 1000, ylim = c(0, 20000),
         xlab = "Numero di commenti per video", main = "Istogramma dei commenti", col = "orange")
     abline(v = comment_mean, type = "l", col = "yellow", lwd = 2)
     abline(v=com_q[1], col="red", lwd=2) # 0% (min)
     abline(v=com_q[2], col="blue", lwd=2) # 1st quartile 25%
     abline(v=com_q[3], col="green", lwd=2.5) # Median value distribution 50%
     abline(v=com_q[4], col="blue", lwd=2) # 3rd quartile 75%
     abline(v=com_q[5], col="red", lwd=2) # 100% (max)
     legend("topright", legend = c("media", "Minimo (0%)", "Primo quantile (25%)", "Mediana (50%)", "Terzo quantile (75%)", "Massimo (100%)"), 
            lwd =2, col = c("yellow", "red", "blue", "green", "blue", "red"))
  })
  
  
}
  
  if(input$radio_button == "Boxplot"){
    
    show("boxplot_plot1")
    show("boxplot_plot2")
    show("boxplot_plot3")
    show("boxplot_plot4")
    
    hide("hist1")
    hide("hist2")
    hide("hist3")
    hide("hist4")
    hide("qq_plot1")
    hide("qq_plot2")
    hide("qq_plot3")
    hide("qq_plot4")
    hide("corr1")
    hide("corr2")
    hide("vid_tend")
    hide("piu_views")
    hide("piu_likes")
    hide("piu_dislikes")
    
    output$boxplot_plot1 <- renderPlot ({
      par(mfrow = c(2,1))
      boxplot(DFvideo$views, horizontal = TRUE, staplewex = 3, main = "Boxplot views")
      boxplot(DFvideo$views, horizontal = TRUE, outline = FALSE)
    })
    
    output$boxplot_plot2 <- renderPlot ({
      par(mfrow = c(2,1))
      boxplot(DFvideo$likes, horizontal = TRUE, staplewex = 3, main = "Boxplot likes")
      boxplot(DFvideo$likes, horizontal = TRUE, outline = FALSE)
    })
    
    output$boxplot_plot3 <- renderPlot ({
      par(mfrow = c(2,1))
      boxplot(DFvideo$dislikes, horizontal = TRUE, staplewex = 3, main = "Boxplot dislikes")
      boxplot(DFvideo$dislikes, horizontal = TRUE, outline = FALSE)
    })
    
    output$boxplot_plot4 <- renderPlot ({
      par(mfrow = c(2,1))
      boxplot(DFvideo$comment_count, horizontal = TRUE, staplewex = 3, main = "Boxplot commenti")
      boxplot(DFvideo$comment_count, horizontal = TRUE, outline = FALSE)
      
    })
    
  }
  
  if(input$radio_button == "QQPlot"){
    
    show("qq_plot1")
    show("qq_plot2")
    show("qq_plot3")
    show("qq_plot4")
    
    hide("hist1")
    hide("hist2")
    hide("hist3")
    hide("hist4")
    hide("boxplot_plot1")
    hide("boxplot_plot2")
    hide("boxplot_plot3")
    hide("boxplot_plot4")
    hide("corr1")
    hide("corr2")
    hide("vid_tend")
    hide("piu_views")
    hide("piu_likes")
    hide("piu_dislikes")
    
    output$qq_plot1 <- renderPlot ({
      qqnorm(DFvideo$views, pch = 1, frame = FALSE, main = "QQPlot delle views")
      qqline(DFvideo$views, col = "steelblue", lwd = 3)
      
    })
    
    output$qq_plot2 <- renderPlot ({
      qqnorm(DFvideo$likes, pch = 1, frame = FALSE, main = "QQPlot dei likes")
      qqline(DFvideo$likes, col = "pink", lwd = 3)
    })
    
    output$qq_plot3 <- renderPlot ({
      qqnorm(DFvideo$dislikes, pch = 1, frame = FALSE, main = "QQPlot dei dislikes")
      qqline(DFvideo$dislikes, col = "gray", lwd = 3)
    })
    
    output$qq_plot4 <- renderPlot ({
      qqnorm(DFvideo$comment_count, pch = 1, frame = FALSE, main = "QQPlot dei commenti")
      qqline(DFvideo$comment_count, col = "orange", lwd = 3)
    })
    
  }
  
  if(input$radio_button == "Considerazioni generali"){
    
    show("corr1")
    show("corr2")
    show("vid_tend")
    show("piu_views")
    show("piu_likes")
    show("piu_dislikes")
    
    hide("hist1")
    hide("hist2")
    hide("hist3")
    hide("hist4")
    hide("boxplot_plot1")
    hide("boxplot_plot2")
    hide("boxplot_plot3")
    hide("boxplot_plot4")
    hide("qq_plot1")
    hide("qq_plot2")
    hide("qq_plot3")
    hide("qq_plot4")
    
    output$corr1 <- renderPlot ({
      corrplot.mixed(corr=cor(DFvideo[ , c(7:10)],
                              use="complete.obs"),upper="ellipse", tl.pos="lt",
                     lower.col = colorpanel(50, "red", "gray60", "blue4"),
                     upper.col = colorpanel(50, "red", "gray60", "blue4"))
    })
      
    output$corr2 <- renderPlot ({
      pairs(cbind(DFvideo$views, DFvideo$likes, DFvideo$dislikes, DFvideo$comment_count),
            main = "correlazione tra views, likes, dislikes e commenti",
            labels=c("views", "likes", "dislikes", "commenti"))
    })
    
    
    output$vid_tend <- renderPlot ({
      ggplot(ca_df, aes(x = category_id, y = count, fill = factor(category_id))) + geom_bar(stat = "identity") + 
        scale_x_discrete(name = "Categoria") + scale_y_continuous(name = "Numero di video") + 
        labs(title = "Video in tendenza in Canada in base alla categoria") 
    })
    
    output$piu_views <- renderPlot ({
      ggplot(prova_view,
             aes(x = numero_views, y = canale)) +
        geom_point(size = 4, color = c("red")) + 
        labs(x = 'media delle views', y = 'canale')
    })
    
    output$piu_likes <- renderPlot ({
      ggplot(prova_like,
             aes(x = numero_likes, y = canale)) +
        geom_point(size = 4, color = c("blue")) + 
        labs(x = 'media dei likes', y = 'canale')
      
    })
    
    output$piu_dislikes <- renderPlot ({
      ggplot(prova_dis,
             aes(x = numero_dislikes, y = canale)) +
        geom_point(size = 4, color = c("green")) + 
        labs(x = 'media dei dislikes', y = 'canale')
    })
  }
  
  
  
})
  
  
  
  #reccomendation
  
  #cosine similarity

  output$recc_cosine1 <- renderPrint({
    indici_simili_cos <- similarity_cosine_fun(input$indice_target_cos)
    print(c("Cosine similarity (video-target + top 5 video simili):", (indici_simili_cos[1:6, ])), max.levels=0)
  })

#pearson
  
  output$recc_pearson1 <- renderPrint({
    indici_simili_pea <- similarity_pearson_fun(input$indice_target_pea)
    print(c("Pearson correlation (video-target + top 5 video simili):", (indici_simili_pea[1:6, ])), max.levels=0)
  })
  

  
}
