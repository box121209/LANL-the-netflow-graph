library(shiny)
library(vcd)
library(igraph)
library(shape)

load("data/box_profiles_20151217.Rds")
# box sequence is bbox; functional layout is tsne.unwt
# LINE layout is line_lout_12 = 100 cols level 2, 100 cols level 2

source("functions.R")

# the comms graph:
g <- graph.edgelist(as.matrix(edges[,1:2]))
V(g)$size <- 0.1 * log(1 + degree(g))
V(g)$label <- ""

nbyte <- nbyte[,2]
deg <- degree(g)
cex_factor <- 2

shinyServer(
  function(input, output, session) {

    ########################################################################
    # graph layout selection:
    
    comm_coords <- reactive({
      switch(input$commcoords,
             "DRL" = drl_lout,
             "Fruchterman-Reingold" = fr_lout,
             "LINE 1st-order" = line_lout_1,
             "LINE 2nd-order" = line_lout_2,
             "LINE mixed" = line_lout_12
      )
    })

    ########################################################################
    # port selection:
    
    inselect <- reactive({
      if(input$port  == "Any"){ 
        NULL
      } else {
        which(in.port.fprob[, which(ports==input$port)] > 0.0)
      } 
    })
    outselect <- reactive({
      if(input$port == "Any"){ 
        NULL
      } else {
        which(out.port.fprob[, which(ports==input$port)] > 0.0)
      } 
    })
    
    ########################################################################
    # subset selection:
    
    subset_func <- reactive({
      res <- brushedPoints(func_coords, input$func_plot_brush, "x1", "x2")
      if (nrow(res) == 0) return()
      as.numeric(row.names(res))
    })
    subset_comm <- reactive({
      res <- brushedPoints(comm_coords(), input$comm_plot_brush, "x1", "x2")
      if (nrow(res) == 0) return()
      as.numeric(row.names(res))
    })
    commselection <- reactive({ !is.null(subset_comm()) })
    
    output$debug_func <- renderPrint({ cat( subset_func() ) })
    output$debug_comm <- renderPrint({ cat( subset_comm() ) })
    
    ########################################################################
    # selection for zoom graph:
    
    subv <- reactive({
      if(commselection()){ subset_comm() } else { subset_func() }
    })
    currentgraph <- reactive({
      induced.subgraph(g, subv())
    })
    subsetdf <- reactive({
      g <- currentgraph()
      df <- switch(input$zoomcoords,
                   "Communications layout" = comm_coords()[subv(),],
                   "Functional layout" = func_coords[subv(),],
                   "Auto adjust" = data.frame(layout.auto(g))
      )
      row.names(df) <- subv()
      names(df) <- c("x1","x2")
      df
    })
    
    ########################################################################
    # plotting paramters:
    
    col_func <- reactive({
      n <- length(bbox)
      if(input$port  == "Any" & is.null(subset_comm())){
        rep("blue", n)
      } else if(input$port  == "Any"){
        out <-  rep("lightgrey", n)
        out[subset_comm()] <- "blue"
        out
      } else if(is.null(subset_comm())){
        out <-  rep("lightgrey", n)
        out[inselect()] <- "red"
        out[outselect()] <- "limegreen"
        out[intersect(inselect(), outselect())] <- "blue"
        out
      } else{
        out <-  rep("lightgrey", n)
        out[intersect(inselect(), subset_comm())] <- "red"
        out[intersect(outselect(), subset_comm())] <- "limegreen"
        tmp <- intersect(inselect(), outselect())
        out[intersect(tmp, subset_comm())] <- "blue"
        out
      }
    })
    col_comm <- reactive({
      n <- length(bbox)
      if(input$port  == "Any" & is.null(subset_func())){
        rep("blue", n)
      } else if(input$port  == "Any"){
        out <-  rep("lightgrey", n)
        out[subset_func()] <- "blue"
        out
      } else if(is.null(subset_func())){
        out <-  rep("lightgrey", n)
        out[inselect()] <- "red"
        out[outselect()] <- "limegreen"
        out[intersect(inselect(), outselect())] <- "blue"
        out
      } else{
        out <-  rep("lightgrey", n)
        out[intersect(inselect(), subset_func())] <- "red"
        out[intersect(outselect(), subset_func())] <- "limegreen"
        tmp <- intersect(inselect(), outselect())
        out[intersect(tmp, subset_func())] <- "blue"
        out
      }
    })
    cex_func <- reactive({
      n <- length(bbox)
      mn <- min(log(nbyte,2))
      mx <- max(log(nbyte,2))
      out <-  0.01 + (mx - log(nbyte,2))/(mx - mn)/2
      if(input$port  == "Any" & is.null(subset_comm())){
        out
      } else if(input$port  == "Any"){
        out[subset_comm()] <- cex_factor*out[subset_comm()]
        out
      } else if(is.null(subset_comm())){
        out[inselect()] <- 1.5 * out[inselect()]
        out[outselect()] <- 1.5 * out[outselect()]
        out
      } else{
        out[inselect()] <- 1.5 * out[inselect()]
        out[outselect()] <- 1.5 * out[outselect()]
        out[subset_comm()] <- cex_factor*out[subset_comm()]
        out
      }
    })
    cex_comm <- reactive({
      n <- length(bbox)
      out <-  0.1*log(1 + deg)
      if(!is.null(subset_func())){
        out[subset_func()] <- cex_factor*out[subset_func()]
      } 
      out
    })
    pch_func <- reactive({
      n <- length(bbox)
      out <-  rep(22, n)
      if(input$port  != "Any"){
        out[inselect()] <- 15
        out[outselect()] <- 15
      }
      if(!is.null(subset_comm())){
        out[subset_comm()] <- 15
      }
      out
    })
    pch_comm <- reactive({
      n <- length(bbox)
      out <-  rep(1, n)
      if(!is.null(subset_func())){
        out[subset_func()] <- 19
      }
      out
    })
    
    ########################################################################
    # main plot - functional view:
    
    output$funcplot <- renderPlot({
      par(mai=c(0,0,0,0))
      plot(func_coords, cex=cex_func(), col=col_func(), pch=pch_func(), axes=FALSE, frame.plot=TRUE, xlab="", ylab="")
    })
    
    ########################################################################
    # main plot - comms view:
    
    output$commplot <- renderPlot({
      par(mai=c(0,0,0,0))
      handplot(g, comm_coords(), prob=input$edgefrac / 100,
               cex=cex_comm(), col=col_comm(), pch=pch_comm())
    })
    
    ########################################################################
    # zoom graph plot:
    
    output$subplot <- renderPlot({
      if(is.null(input$comm_plot_brush) & is.null(input$func_plot_brush)) return()
      if(!input$drawzoom) return()
      g <- currentgraph()
      col <- if(commselection()) col_comm()[subv()] else col_func()[subv()]
      handsubplot(g, subsetdf(), col=col)
    })
    
    ########################################################################
    # protocol temperature:
    out.proto.f.feat <- reactive({
      out.proto.fprob^(1/input$proto_temp)
    })
    out.proto.b.feat <- reactive({
      out.proto.bprob^(1/input$proto_temp)
    })
    out.proto.s.feat <- reactive({
      out.proto.sprob^(1/input$proto_temp)
    })
    in.proto.f.feat <- reactive({
      in.proto.fprob^(1/input$proto_temp)
    })
    in.proto.b.feat <- reactive({
      in.proto.bprob^(1/input$proto_temp)
    })
    in.proto.s.feat <- reactive({
      in.proto.sprob^(1/input$proto_temp)
    })
    
    ########################################################################
    # protocol profile plot:
    
    show.box.proto <- function(idx){
      
      if(is.null(idx)) return()
      b <- as.character( bbox[idx] )
      tmp <- data.frame( rbind(
        in.proto.f.feat()[idx,], out.proto.f.feat()[idx,],
        in.proto.b.feat()[idx,], out.proto.b.feat()[idx,],
        in.proto.s.feat()[idx,], out.proto.s.feat()[idx,]
      ) )
      names(tmp) <- c("ICMP","UDP","TCP","41")
      row.names(tmp) <- c("flows", "", "bytes", " ", "time", "  ")
      col <- matrix(rep( c(
        c("red4","red","pink","lavender"),
        c("springgreen4","limegreen","lawngreen","yellow")
      ),3), nrow=6, ncol=4, byrow=TRUE)
      mos <- mosaic(as.matrix(tmp), 
                    gp = gpar(fill=col), 
                    labeling = TRUE, 
                    direction="v",
                    las=2, cex.axis=1.5, 
                    main=sprintf("Computer %s", b))
    }
    
    ########################################################################
    # port temperature:
    
    out.port.f.feat <- reactive({
      out.port.fprob^(1/input$port_temp)
    })
    out.port.b.feat <- reactive({
      out.port.bprob^(1/input$port_temp)
    })
    out.port.s.feat <- reactive({
      out.port.sprob^(1/input$port_temp)
    })
    in.port.f.feat <- reactive({
      in.port.fprob^(1/input$port_temp)
    })
    in.port.b.feat <- reactive({
      in.port.bprob^(1/input$port_temp)
    })
    in.port.s.feat <- reactive({
      in.port.sprob^(1/input$port_temp)
    })
    
    ########################################################################
    # port profile plot:
    
    show.box.port <- function(idx){
      
      if(is.null(idx)) return()
      b <- as.character( bbox[idx] )
      mx <- max( which(in.port.f.feat()[idx,] > 0 | out.port.f.feat()[idx,] 
                       | in.port.b.feat()[idx,] > 0 | out.port.b.feat()[idx,]
                       | in.port.s.feat()[idx,] > 0 | out.port.s.feat()[idx,]) )
      if(mx < length(ports)) mx <- mx + 1
      index <- 1:mx
      outdex <- index+0.5
      threshold <- 0.05
      lab.in <- rep("", mx)
      lab.out <- rep("", mx)
      cond.in <- ( in.port.f.feat()[idx,1:mx] > threshold  )
      cond.out <- ( out.port.f.feat()[idx,1:mx] > threshold )
      lab.in[cond.in] <- ports[1:mx][cond.in]
      lab.out[cond.out] <- ports[1:mx][cond.out]
      lwd <- 5
      par(mfrow=c(3,1), mai=c(0,0,0,0))
      plot(index, in.port.f.feat()[idx,1:mx],
           main="",
           xlab="",
           ylab = "",
           type='h', 
           ylim=c(0,1), 
           lwd=lwd, col='red', 
           yaxt='n',
           xaxt='n')
      in.txtht <- (in.port.f.feat()[idx,1:mx] + 1)/2
      out.txtht <- (out.port.f.feat()[idx,1:mx] + 1)/2
      text(index, in.txtht, labels=lab.in, cex=1.5, col='black')
      points(out.port.f.feat()[idx,1:mx] ~ outdex, 
             main=sprintf("Computer %s", b),
             type='h', 
             lwd=lwd, col='green')
      text(outdex, out.txtht, labels=lab.out, cex=1.5, col='black')
      legend("topright", bty='n', cex=1.5, legend="Flows", text.col="grey")
      plot(index, in.port.b.feat()[idx,1:mx],
           main="",
           xlab="",
           ylab = "",
           type='h', 
           ylim=c(0,1), 
           lwd=lwd, col='red', 
           yaxt='n',
           xaxt='n')
      legend("topright", bty='n', cex=1.5, legend="Bytes", text.col="grey")
      points(out.port.b.feat()[idx,1:mx] ~ outdex, 
             main=sprintf("Computer %s", b),
             type='h', 
             lwd=lwd, col='green')
      plot(index, in.port.s.feat()[idx,1:mx],
           main="",
           xlab="",
           ylab = "",
           type='h', 
           ylim=c(0,1), 
           lwd=lwd, col='red', 
           yaxt='n',
           xaxt='n')
      lab.in <- rep("", mx)
      lab.out <- rep("", mx)
      cond.in <- ( in.port.s.feat()[idx,1:mx] > threshold  )
      cond.out <- ( out.port.s.feat()[idx,1:mx] > threshold )
      lab.in[cond.in] <- ports[1:mx][cond.in]
      lab.out[cond.out] <- ports[1:mx][cond.out]
      in.txtht <- (in.port.s.feat()[idx,1:mx] + 1)/2
      out.txtht <- (out.port.s.feat()[idx,1:mx] + 1)/2
      text(index, in.txtht, labels=lab.in, cex=1.5, col='black')
      points(out.port.s.feat()[idx,1:mx] ~ outdex, 
             main=sprintf("Computer %s", b),
             type='h', 
             lwd=lwd, col='green')
      text(outdex, out.txtht, labels=lab.out, cex=1.5, col='black')
      legend("topright", bty='n', cex=1.5, legend="Seconds", text.col="grey")
    }
    
    ########################################################################
    # hover-over for box profiles:
    
    idx <- reactive({
      res_click <- nearPoints(subsetdf(), input$plot_click, 
                              "x1", "x2", threshold=10, maxpoints=1)
      res_hover <- nearPoints(subsetdf(), input$plot_hover, 
                              "x1", "x2", threshold=10, maxpoints=1)
      if(nrow(res_click)>0){ as.numeric(row.names(res_click)) }
      else if(nrow(res_hover)>0){ as.numeric(row.names(res_hover)) }
      else NULL
    })
    output$plot_hover_proto <- renderPlot({
      show.box.proto(idx())
    })
    output$plot_hover_port <- renderPlot({
      show.box.port(idx())
    })
  }
)

