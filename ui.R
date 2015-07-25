
shinyUI(fluidPage(
    titlePanel("Interactive Statistical Analysis"),
    h5("The Power of R for your data...Graphically!"),
    fluidRow(
        column(3, wellPanel(h4("User Inputs"),
            selectInput("dSet", "Select Dataset", multiple=FALSE,
                        choices=c("NY Airquality" = "airquality",
                                  "US State Facts" = "state",
                                  "Motor Trend Car Road Tests" = "mtcars",
                                  "Average Life Savings" = "LifeCycleSavings",
                                  "Esophageal Cancer" = "esoph")),
            uiOutput("yVarControl"),
            uiOutput("xVarControl"),
            selectInput("model", "Select Model to Fit", multiple=FALSE,
                        choices=c("linear regression" = "lm",
                                  "logistic regression" = "glm")),
            conditionalPanel(
                condition = "input.model == 'glm'",
                radioButtons("glmFamily", "Distribution Family",
                             choices=c("gaussian", "binomial", "poisson"))
            ),
            checkboxInput("intercept", "Use Intercept in Model", value=TRUE),
            actionButton(inputId="go1", label="Build Models")
        )),
        column(9, tabsetPanel(
            tabPanel("Data Explorer", p("\n\n"),
                fluidRow(
                    column(3, uiOutput("animXVar") ),
                    column(9, align="center", uiOutput("animSlider") )
                ),
                fluidRow(
                    column(3, uiOutput("corrChartType")),
                    column(9, align="center", h4(textOutput("anim_title")),
                        conditionalPanel(condition = "input.corrChart == 'heatmap'",
                            plotOutput("userAnim1")),
                        htmlOutput("userAnim2"))
                ),
                fluidRow(column(12, align="center",
                        h4("6-Number Summary of All Variables in the Data"),
                        p(verbatimTextOutput("summDSet"))
                    )
                )
            ),
            tabPanel("Model Builder", p("\n\n"),
                fluidRow(
                    column(3, h4("User Selections"), wellPanel(
                        p(strong("Dateset : "), textOutput("dSet_out", inline=TRUE)),
                        p(strong("Dependent Variable : "), textOutput("yVar_out",
                                                                 inline=TRUE)),
                        p(strong("Predictors : "), textOutput("xVar_out")),
                        p(strong("Model To Fit : "), textOutput("model_out", inline=TRUE))
                    )),
                    column(9, align="center", h4("Model Output"),
                        p(verbatimTextOutput("userModel_summ")),
                        htmlOutput("userModel_plot")
                    )
                )
            ),
            tabPanel("Documentation", p("\n\n"),
                p("This is intended to be a fully operational, web-only platform for 
                  sophisticated statistical analysis, using R at the backend.", br(),
                  "We expect our users to have beginner-level knowledge of statistial
                  methods, and possibly very little to no programming experience.", br(),
                  "However, we intend to create, maintain and expand a statistics knowledge 
                  centre for users that can be used by all for easy reference."),
                br(),
                p("We will explain the features of the tabs currently available to users, 
                  use an example to provide the sequential steps needed for analysis 
                  (a workflow, if you will), and interpretation of the results.", br(),
                  "We will conclude with a list of known issues and planned features
                  (using your own data instead of sample data, creating and saving a 
                  workflow for reuse, etc.).", br(),
                  "You can also refer to this ",
                  a(strong("presentation"), href="http://anirbanc81.github.io/Coursera_DataProds_Project/InteractiveStats"),
                  " for a summary of available features, planned features with tentative 
                  release schedule, and general info on our initiative."),
                br(), br(),
                h4("1. User Inputs Section"),
                p("This is the left-most gray panel titled ", strong("User Inputs"), ". It 
                  is visible across all tabs for your easy reference."),
                br(),
                h5(strong("1.1 Select Dataset")),
                p("This is a single-select dropdown list. Currently it is populated with 
                  5 sample datasets from the R ", strong("datasets package"), ": "),
                HTML('<ul style="list-style-type:circle">
                        <li>airquality - NY Airquality</li>
                        <li>amalgamation of state.abb, state.division, state.x77 - US State Facts</li>
                        <li>mtcars - Motor Trend Car Road Tests</li>
                        <li>LifeCycleSavings - Average Life Savings</li>
                        <li>esoph - Esophageal Cancer</li>
                     </ul>'),
                p("Choice of dataset controls the data exploration options available in 
                  the 'Data Explorer' tab.", br(),
                  "For all datasets except 'US State Facts', a variable-correlation bubble 
                  chart is shown, while for the 'US State Facts' a geo-chart is used."),
                br(),
                h5(strong("1.2 Variable To Predict")),
                p("These are the variables (i.e. columns) present in the dataset you 
                  selected in ", strong("1.1"), " above. You can select any one of these as 
                  the target (dependent, what you are interested in predicting) variable, 
                  but only one can be selected at a time.", br(),
                  "Your choice of this variable controls your options in ", strong("1.3"),
                  " below, and also the display of the geo-chart in the 'Data Explorer' 
                  tab ", strong("(Section 2)"), "."),
                br(),
                h5(strong("1.3 Predictor Variables")),
                p("All variables in the dataset selected in ", strong("1.1"), " except the 
                  variable to predict selected in ", strong("1.2"), " above.", br(),
                  "This is a multi-select input, and the idea is to select the variables most 
                  correlated to the variable to predict, and have the least correlation 
                  between themselves.", br(),
                  "As an alternative, choose the 'Use All' option and sequentially remove the 
                  least significant variables."),
                br(),
                h5(strong("1.4 Select Model To Fit")),
                p("This is where you tell the system what type of model you think might best 
                  explain the variable to predict using the predictor variables. This is a single 
                  select field, but you can run different models and then choose the best one.", br(),
                  "The ", em("'Use Intercept in Model'"), " checkbox lets you specify if an 
                  additive constant is relevant for the model.", br(),
                  "If you select the 'logistic regression' option, you will also need to choose 
                  the distribution that best represents the process by which the data was created, 
                  i.e. a flip of a coin is binomial, or the rate at which diners arrive at a 
                  restaurant is poisson, etc. etc."),
                br(), br(),
                h4("2. Data Explorer Tab"),
                p("This is the first tab at the top of the page. It is the default tab visible 
                  when you first open the page. Before you continue on to build your model(s), 
                  we encourage you to improve your understanding of the data using this tab.", br(),
                  "There are 2 panels in this tab -"),
                HTML('<ul style="list-style-type:disc">
                        <li>the top one lets you explore and understand the relation/correlation 
                            between the variables in the data</li>
                        <li>the bottom one provides the type of values in each variable, and a 
                            summary of the values themselves</li>
                     </ul>'),
                br(),
                h5(strong("2.1 Data Explorer - Top Panel")),
                p("The display in this section is controlled by your choice of dataset (refer ",
                  strong("Section 1.1"), " also)."),
                HTML('<ul style="list-style-type:circle">
                        <li><b>US State Facts</b> - Geo-chart plotting the dependent variable 
                            (<b>Section 1.2</b>) by US states. You can select one of the 
                            numerical independent variables (<b>Section 1.3</b>) from the 
                            dropdown titled <b>Animate Plot By</b> and change the slider value 
                            to see the distribution of the dependent variable across states 
                            <u>that have values less than the slider value for the independent 
                            variable</u> you chose. You can even press the PLAY button at the 
                            bottom-right of the slider and see an animated update of the chart.
                        </li>
                        <li>All others - A bubble chart of the pairwise 
                            correlation values between all numerical variables in the dataset. The 
                            plot axes are numbered variables, with the legend indicating the color 
                            scheme for each variable, and the bubble size indicating the absolute 
                            correlation value (we are interested in unearthing strong correlation, 
                            irrespective of the sign of the connection).
                        </li>
                     </ul>'),
                br(),
                h5(strong("2.2 Data Explorer - Bottom Panel")),
                HTML("This section provides the <u>6-Number summary for each numerical 
                    variable</u> (Minimum, 1st Quartile, Median, Mean, 3rd Quartile, Maximum) 
                    in the data, or <u>frequency of the first 6 levels of factor variables</u>.
                    <br/>For all types of variables, the count of NA values are also shown."),
                br(), br(), br(), br(),
                h4("3. Model Builder Tab"),
                p("This tab follows the Data Explorer tab at the top of the page and has 
                  a similar layout with a top and a bottom panel. The top panel has a 
                  sub-panel at the left that summarises the user inputs used in building 
                  the model for your reference.", br(),
                  "This is the tab where all the action takes place when you click the ",
                  strong(em("Build Models")), " button in the User Inputs section at the left 
                  of the page (refer ", strong("Section 1"), " for selecting the inputs)."),
                HTML('<ul style="list-style-type:disc">
                        <li>the top panel generates a summary report of the model fitted, 
                            along with statistics to quantify the goodness of fit</li>
                        <li>the bottom panel creates a plot of the actual values of the 
                            variable to predict and the corresponding predictions from the 
                            model, and shows a reference line of slope 1 to indicate the 
                            ideal scenario of 100% accuracy</li>
                     </ul>'),
                br(), br(), br(), br(),
                h4("Appendix"), br(),
                h5("A1 - List of known issues"), br(),
                h5("A2 - New Features Release Schedule (Tentative)"),
                br(), br(),
                p("We hope that our efforts will help to familiarize you with the amazing 
                  platform that is ", a(strong("R"), href="http://www.r-project.org/"),
                  ", and introduce you to the ",
                  a(strong("RStudio IDE"), href="http://www.rstudio.com/"), " that will 
                  make you a R Jedi!"),
                h6("Comments/Feedback/Bug Reports are gladly welcomed at ",
                   strong("anirban<dot>c81<at>gmail")),
                br(), br()
            )
        ))
    )
))
