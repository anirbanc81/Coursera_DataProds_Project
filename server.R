
library(shiny); library(shinyapps); library(datasets); library(rpart); library(rpart.plot)
library(reshape2); library(googleVis); library(gtools)

shinyServer(
    function(input, output, session) {
        output$dSet_out <- renderText({input$dSet})
        dataSelect <- reactive({
            switch(input$dSet,
                   "airquality" = airquality,
                   "state" = data.frame(state = state.abb,
                                        division = as.character(state.division), state.x77),
                   "mtcars" = mtcars,
                   "LifeCycleSavings" = LifeCycleSavings,
                   "esoph" = esoph)
        })
        varNames <- reactive({ colnames(dataSelect()) })
        
        output$yVarControl <- renderUI({
            radioButtons("yVar", "Variable To Predict", choices=varNames())
        })
        output$yVar_out <- renderText({input$yVar})
        output$yVar_out1 <- renderText({input$yVar})
        
        output$xVarControl <- renderUI({
            checkboxGroupInput("xVar", "Predictor Variables", selected = "Use All",
                               choices=c("Use All", varNames()[varNames() != input$yVar]))
        })
        userXVars <- reactive({
            if (input$go1 > 0) {
                if (length(input$xVar) == 0) { varNames()[varNames() != input$yVar]
                } else if (any(input$xVar[input$xVar != input$yVar] == "Use All")) {
                    varNames()[varNames() != input$yVar]
                } else { input$xVar }
            }
        })
        output$xVar_out <- renderText({
            if (input$go1 > 0) { isolate(
                if (length(input$xVar) == 0) {
                    updateCheckboxGroupInput(session, "xVar", selected="Use All")
                    paste(userXVars(), collapse=", ")
                } else { paste(userXVars(), collapse=", ") }
            )}
        })
        
        output$model_out <- renderText({
            if (input$model == "glm") { paste(input$model, input$glmFamily)
            } else { input$model }
        })
        
        userFormula <- reactive({
            if (input$intercept) {
                as.formula(paste(input$yVar, "~", paste(userXVars(), collapse=" + ")))
            } else {
                as.formula(paste(input$yVar, "~ 0 + ", paste(userXVars(), collapse=" + ")))
            }
        })
        userModel <- reactive({
            if (input$go1 > 0) { isolate(
                if (input$model == "lm") {
                    lm(userFormula(), data=dataSelect(), na.action=na.exclude, y=TRUE)
                } else if (input$model == "glm") {
                    glm(userFormula(), data=dataSelect(), family=input$glmFamily,
                        na.action=na.exclude)
                } else { paste("WIP") }
            )}
        })
        
        output$userModel_summ <- renderPrint({
            if (input$go1 > 0) { isolate(
                summary(userModel())
            )}
        })
        output$userModel_plot <- renderGvis({
            if (input$go1 > 0) {
            gvisScatterChart(
                data.frame(actual=userModel()$y, predicted=userModel()$fitted.values,
                            ref_slope_1=userModel()$y),
                 options=list(
                     title="Prediction Accuracy", hAxis="{title:'Actual'}",
                     vAxis="{title:'Predicted'}",
                     series="{
                         1: {color:'red', pointSize:0, lineWidth:1}
                     }",
                     height=400)
            )}
        })
        
        numericVarNames <- reactive({
            colnames(dataSelect())[sapply(dataSelect(), FUN=is.numeric)]
        })
        output$animXVar <- renderUI({
            if (input$dSet == "state" && input$yVar != "state" && input$corrChart == "geo") {
                selectInput("animX", "Animate Plot By", multiple=FALSE,
                            choices=numericVarNames()[numericVarNames() != input$yVar]) }
        })
        
        animColNum <- reactive({
            if (input$dSet == "state" && input$yVar != "state") {
                grep(input$animX, colnames(dataSelect()), fixed=TRUE, useBytes=TRUE)[1] }
        })
        animXMin <- reactive({
            if (input$dSet == "state" && input$yVar != "state") {
                round(min(dataSelect()[, animColNum()]), 2) }
        })
        animXMax <- reactive({
            if (input$dSet == "state" && input$yVar != "state") {
                round(max(dataSelect()[, animColNum()]), 2) }
        })
        output$animSlider <- renderUI({
            if (input$dSet == "state" && input$yVar != "state" && input$corrChart == "geo") {
                sliderInput("animXVals", "Select Value or Press Play to Animate",
                    min=animXMin(), max=animXMax(), value=animXMax(),
                    step=(animXMax() - animXMin())/10, animate=TRUE
            )}
        })
        animSliderVal <- reactive({
            input$animXVals
        })
        
        output$corrChartType <- renderUI({
            if (input$dSet=="state" && input$yVar!="state") {
                selectInput("corrChart", "Select Chart Type", multiple=FALSE,
                            choices=c("Geo Chart" = "geo",
                                      "Correlation Heatmap" = "heatmap"))
            } else if (input$dSet != "state") {
                selectInput("corrChart", "Select Chart Type", multiple=FALSE,
                            choices=c("Correlation Bubble Chart" = "bubble",
                                      "Correlation Heatmap" = "heatmap"))
            }
        })
        
        output$anim_title <- renderText({
            if (input$dSet == "state" && input$yVar != "state") {
                if (input$corrChart == "geo") {
                    paste("Distribution of", input$yVar, "by state, filtered by", input$animX)
                } else if (input$corrChart == "heatmap") {
                    paste("Spearman Correlation of Variables (numeric only) in ", input$dSet)
                }
            } else if (input$dSet == "state" && input$yVar == "state") {
                paste("Select another variable. Cannot map State to State!")
            } else if (input$dSet != "state" && length(numericVarNames()) > 0) {
                paste("Spearman Correlation of Variables (numeric only) in ", input$dSet)
            } else {paste("Data has only categorical variables")}
        })
        output$userAnim1 <- renderPlot({
            if(input$dSet!="" && input$corrChart=="heatmap" && length(numericVarNames())>0) {
                heatmap(abs(cor( dataSelect()[, numericVarNames()], method="spearman",
                                 use="complete.obs" )))
            }
        })
        output$userAnim2 <- renderGvis({
        if(input$dSet!="" && (input$corrChart=="geo" || input$corrChart=="bubble")) {
            if (input$dSet == "state" && input$yVar != "state") {
                rawData <- dataSelect()
                plotData <- rawData[rawData[, animColNum()] <= animSliderVal(), ]
                gvisGeoChart(
                    plotData, locationvar="state", colorvar=input$yVar,
                    options=list(
                        region="US", displayMode="regions", resolution="provinces",
                        colorAxis="{colors:['#FFFFFF', '#0000FF']}", height=400)
                )
            } else if (input$dSet != "state" && length(numericVarNames()) > 0) {
                rawData <- dataSelect()[numericVarNames()]
                varsCor <- as.data.frame(abs(cor(rawData, method="spearman",
                                                 use="complete.obs")))
                corData <- data.frame(vars=row.names(varsCor), varsCor, row.names = NULL)
                corMelt <- melt(corData, id.vars = "vars", value.name = "correlation")
                
                corMelt$plotX <- rep(1:nrow(corData), times=nrow(corData))
                corMelt$plotY <- rep(1:nrow(corData), each=nrow(corData))
                corMelt$ids <- paste(corMelt$vars, corMelt$variable, sep="_")
                corMelt$correlation[corMelt$vars == corMelt$variable] <- 0
                
                plotData <- corMelt[corMelt$plotX <= corMelt$plotY, ]
                plotData <- plotData[!is.na(corMelt$correlation), ]
                gvisBubbleChart(
                    plotData, idvar="ids", xvar="plotX", yvar="plotY", colorvar="vars",
                    sizevar="correlation",
                    options=list(sizeAxis="{minValue:0, maxSize:15}",
                                 hAxis="{minValue:0}", vAxis="{minValue:0}",
                                 height=400)
                )
            }
        }
        })
        output$summDSet <- renderPrint({ summary(dataSelect()) })
        
    }
)