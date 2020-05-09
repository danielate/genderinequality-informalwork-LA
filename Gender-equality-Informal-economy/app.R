# This is my shiny app 

# First I load the libraries 

library(shiny)
library(shinythemes)
library(readxl)
library(janitor)
library(gt)
library(rvest)
library(reprex)
library(stringr)
library(infer)
library(scales)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(hablar)
library(wesanderson)

gii_latam_s <- read_rds("gii_latam.rds")
gii_can_s <- read_rds("gii_can.rds")

informal_latam_s <- read_rds("informal_latam.rds")
informal_can_s <- read_rds("informal_can.rds")

join_s <- read_rds("data_can_join.rds")

gii_latam_long_s <- read_rds("gii_latam_long.rds")


ui <- fluidPage(
    # I add a theme
    theme = shinytheme("journal"),
    
    navbarPage(
        tags$b("Gender Equality and the Informal Economy: An analysis of the Andean Community Countries"),
        
# TAB 1: Background

        tabPanel(
            "Background",
            br(),
            
            imageOutput("can", width = "100%", height = "100%"), 
            br(),
            
            p(tags$em("Photograph: Andean Community Countries"), align = "center"),
            br(),
            
            h3("The Andean Community"),
            br(),
            
            fluidRow(column(2), column(8,
           
                                                                 
            p(
                "The Andean Community (Comunidad Andina, CAN) is an international organization 
                made up by Bolivia, Colombia, Ecuador, and Peru. It exists since 1969 and the 
                goal is to achieve economic, political, and social cooperation. These countries 
                where selected as the sample for this analysis because they share similar cultures, 
                challenges, and resources, besides their existing aim of cooperation through the 
                CAN organization. The Andean Community has 98 million inhabitants living in an 
                area of 4,700,000 square kilometers, whose Gross Domestic Product for 2011 
                amounts to US$902.86 billion.
                
                These project aims to visualize the possible correlation between the percentage of women 
                working in the informal economy in the countries of the CAN conmmunity and the gender inequality
                index."
            ))),
            br(),
            
            h3("Overview of the Gender Inequality Index in Latin America"), 
            br(),
            
            plotOutput("giilatam"),
            br(),
            
            h3("Overview of the Percentage of Women in Informal Employment in Latin America"), 
            br(),
            
            plotOutput("informalitylatam"),
            br(),
            
            h4(tags$b("Supporting women in the informal economy takes us closer to the 
                      goal of gender equity!"))),
        
# TAB 2: Gender Inequality Index 

        tabPanel("Gender Inequality Index",
                 fluidPage(titlePanel("Score over time"),
                           br(),
                           p(tags$em("Select an Andean Community Country")),
                             selectInput("countryInput", "Country", c("Bolivia",
                                                                      "Colombia",
                                                                      "Ecuador",
                                                                      "Peru"), multiple = FALSE),
            
                           mainPanel(plotOutput(
                               knitr::include_graphics("graphics/percentage_time.png")
                           ))
                           )), 

# TAB 3: Proportion of Informal Employment 

        tabPanel("Proportion of Informal Employment",
                 fluidPage(titlePanel("Percentage over time"),
                           br(),
                           p(tags$em("Select an Andean community Country")),
                           selectInput("countryInput", "Country", c("Bolivia",
                                                                    "Colombia",
                                                                    "Ecuador",
                                                                    "Peru"), multiple = FALSE),
                           
                           mainPanel(plotOutput(
                               knitr::include_graphics("graphics/percentage_time.png")
                           ))
                           )),
 
# TAB 4: Correlation 


        tabPanel("Correlation",
                 fluidPage(titlePanel("Linear Regression"),
                           mainPanel(plotOutput(
                               knitr::include_graphics("graphics/percentage_time.png")
                           )))),

# TAN 5: About 

        tabPanel("About",
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
            
            p("Source:"), p(tags$a("http://hdr.undp.org/en/content/gender-inequality-index")),
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
            
            p("Source:"), p(tags$a("https://www.ilo.org/shinyapps/bulkexplorer38/?lang=en&segment=indicator&id=SDG_A831_SEX_RT_A")),
            br(),
            
            h3("About me"),
            p("Daniela Teran"),

            tags$img(src='./daniela.jpg', height=150, width=150),
            
            br(),
            
            p( "I am a Master in Design Engineering student, interested in capacity building for low-income workers. 
               This past year I have been working on the design of a development program, focused on creative capacity building 
               for women workers in the informal economy in Latin America. This data project aims to support the design of that
               program.
               
               For more information: dteran@mde.harvard.edu"),
               
               p(tags$a("https://github.com/danielate"))
                
            ))))
    
server <- function(input, output, session) {
    
    output$can <- renderImage({
        list(src = 'Gender-equality-Informal-economy/Andean_Community.png',
             height = 500,
             width = 500,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE)
    
    
    output$giilatam <- renderPlot({
        
        gii_latam_long_s %>%
        
        ggplot(aes(
            x = country,
            y = gii) + geom_col()
        ) 
    })
    
}


shinyApp(ui, server)