# This is my shiny app 

# First I load the libraries 

library(shiny)
library(shinythemes)
library(readxl)
library(janitor)
library(gt)
library(rvest)
library(reprex)
library(fivethirtyeight)
library(stringr)
library(shinythemes)
library(infer)
library(scales)
library(tidyverse)

ui <- fluidPage(
    # I add a theme
    theme = shinytheme("journal"),
    
    navbarPage(
        tags$b("Gender Equality and the Informal Economy in Latin America"),
        
        # I create the layout for the first tab
        tabPanel(
            "Background",
            imageOutput("image", width = "100%", height = "100%"),
            br(),
            
            p(tags$em("Photograph: Artisanal goldminers in Sechocha, Peru."), align = "center"),
            br(),
            
            h3("Project Background and Motivations"),
            br(),
            
            p(
                "For the past year I have been working on the design of Mink'a: 
                A Creative Capacities Development Program for women informal workers 
                in Latin America. This data analysis project aims support that work. 
                It suggests a correlation between gender inequality and women working in 
                the informal economy in Latin America.The goal is to analyze how this 
                already vulnerable group is restricted by issues of gender inequality."
            ),
            br(),
            h4(tags$b("Supporting women in the informal economy takes us closer to the 
                      goal of gender equity!"))),
        
        
                 
        tabPanel("Gender Inequality Index",
                 fluidPage(titlePanel("Score over time"),
                           br(),
                           p(tags$em("Select a Latin American Country")),
                             selectInput("countryInput", "Country", c("Argentina",
                                                                      "Bolivia",
                                                                      "Brazil",
                                                                      "Chile",
                                                                      "Colombia",
                                                                      "Costa Rica",
                                                                      "Dominican Republic",
                                                                      "Ecuador",
                                                                      "El Salvador",
                                                                      "Guatemala",
                                                                      "Guyana",
                                                                      "Haiti",
                                                                      "Honduras",
                                                                      "Jamaica",
                                                                      "Mexico",
                                                                      "Nicaragua",
                                                                      "Panama",
                                                                      "Paraguay",
                                                                      "Peru",
                                                                      "Saint Lucia",
                                                                      "Suriname",
                                                                      "Trinidad and Tobago",
                                                                      "Uruguay",
                                                                      "Venezuela"), multiple = TRUE),
            
                           mainPanel(plotOutput(
                               knitr::include_graphics("graphics/percentage_time.png")
                           )))), 
    
        tabPanel("Proportion of Informal Employment",
                 fluidPage(titlePanel("Percentage over time"),
                           br(),
                           p(tags$em("Select a Latin American Country")),
                           selectInput("countryInput", "Country", c("Argentina",
                                                                    "Bolivia",
                                                                    "Brazil",
                                                                    "Chile",
                                                                    "Colombia",
                                                                    "Costa Rica",
                                                                    "Dominican Republic",
                                                                    "Ecuador",
                                                                    "El Salvador",
                                                                    "Guatemala",
                                                                    "Guyana",
                                                                    "Haiti",
                                                                    "Honduras",
                                                                    "Jamaica",
                                                                    "Mexico",
                                                                    "Nicaragua",
                                                                    "Panama",
                                                                    "Paraguay",
                                                                    "Peru",
                                                                    "Saint Lucia",
                                                                    "Suriname",
                                                                    "Trinidad and Tobago",
                                                                    "Uruguay",
                                                                    "Venezuela"), multiple = TRUE),
                           
                           mainPanel(plotOutput(
                               knitr::include_graphics("graphics/percentage_time.png")
                           )))),
        
        tabPanel("Correlation",
                 fluidPage(titlePanel("Linear Regression"),
                           mainPanel(plotOutput(
                               knitr::include_graphics("graphics/percentage_time.png")
                           )))),
        
        tabPanel("Data Overview",
                 fluidPage(
            
            h3("Gender Innequality Index"),
            p("United Nations Development Programme"),
            br(),
            
            p(
                "Gender inequality remains a major barrier to human development.
                Girls and women have made major strides since 1990, but they have
                not yet gained gender equity. The disadvantages facing women and
                girls are a major source of inequality. All too often, women and
                girls are discriminated against in health, education, political
                representation, labour market, etc.—with negative consequences
                for development of their capabilities and their freedom of choice.
                The GII is an inequality index. It measures gender inequalities in
                three important aspects of human development—reproductive health,
                measured by maternal mortality ratio and adolescent birth rates;
                empowerment, measured by proportion of parliamentary seats occupied
                by females and proportion of adult females and males aged 25 years
                and older with at least some secondary education; and economic status,
                expressed as labour market participation and measured by labour force
                participation rate of female and male populations aged 15 years and older.
                The GII is built on the same framework as the IHDI—to better expose
                differences in the distribution of achievements between women and men.
                It measures the human development costs of gender inequality. Thus the
                higher the GII value the more disparities between females and males and
                the more loss to human development.
                The GII sheds new light on the position of women in 162 countries; it yields
                insights in gender gaps in major areas of human development. The component
                indicators highlight areas in need of critical policy intervention and it
                stimulates proactive thinking and public policy to overcome systematic
                disadvantages of women."),
            br(),
            
            p("Source: http://hdr.undp.org/en/content/gender-inequality-index"),
            br(),
            
            h3("Proportion of Informal Employment in non-agricultural employment – Harmonized series (%)"),
            p("International Labor Organization "),
            br(),
            
            p(
                "This indicator conveys the share of informal employment in total employment 
                in the non-agricultural sector. Employment comprises all persons of working 
                age who, during a specified brief period, were either in paid employment 
                (whether at work or with a job but not at work) or in self-employment (whether 
                at work or with an enterprise but not at work). Informal employment comprises 
                persons who in their main or secondary jobs were (a) own-account workers, 
                employers and members of producers' cooperatives employed in their own informal 
                sector enterprises; (b) own-account workers engaged in the production of goods 
                exclusively for own final use by their household (e.g. subsistence farming); 
                (c) contributing family workers, regardless of whether they work in formal or 
                informal sector enterprises; or (d) employees holding informal jobs, whether 
                employed by formal sector enterprises, informal sector enterprises, or as paid 
                domestic workers by households."),
            
            p("Source: https://www.ilo.org/shinyapps/bulkexplorer38/?lang=en&segment=indicator&id=SDG_A831_SEX_RT_A"),
            br()
            ))))
    
server <- function(input, output, session) {
    
    output$image <- renderImage({
        list(
            src = './graphics/secocha.png',
            height = 400,
            width = 400,
            style = "display: block; margin-left: auto; margin-right: auto;"
        )
    }, deleteFile = FALSE)
    
    output$line_plot <- renderPlot({
            # generate type based on input$plot_type from ui
            ifelse(
                input$plot_type == "a",
                # if input$plot_type is "a", plot histogram of "waiting" column 
                # from the faithful dataframe
                x   <- faithful[, 2],
                # if input$plot_type is "b", plot histogram of "eruptions" column
                # from the faithful dataframe
                x   <- faithful[, 1]
            )
            # draw the histogram with the specified number of bins
            hist(x, col = 'darkgray', border = 'white')
        })
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    