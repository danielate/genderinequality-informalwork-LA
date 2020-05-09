# Shiny App that visuali

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

gii_latam_long_s <- read_rds("gii_latam_long.rds")
informal_latam_long_s <- read_rds("informal_latam_long.rds")

gii_can_long_s <- read_rds("gii_can_long.rds")
informal_can_long_s <- read_rds("informal_can_long.rds")

join_can_s <- read_rds("join_can.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Add a shinytheme for the app ui
    
    theme = shinytheme("journal"),

    # Application title
    
    navbarPage(
        tags$b("Gender Equality and the Informal Economy: 
               An analysis of the Andean Community Countries"),
        
    # TAB 1: Background
    
    tabPanel(
        "Background",
        br(),
        
        imageOutput("cover", width = "100%", height = "100%"), 
        br(),
        
        h3("The Andean Community"),
        br(),
        
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
        ),
        br(),
        
        h3("Overview of the Gender Inequality Index in Latin America"), 
        br(),
        
        # Show a plot Latam GII Index
       
        plotOutput("giilatam"),
        br(),
        
        h3("Overview of the Percentage of Women in Informal Employment in Latin America"), 
        br(),
    
        plotOutput("informallatam"),
        br()
        
        ),
    
    # TAB 2: Gender Inequality Index 
    
    tabPanel("Gender Inequality Index",
             fluidPage(titlePanel("Score over time"),
                       br(),
                       p(tags$em("Select an Andean Community Country")),
                       selectInput("countrycanInput", "Country", c("Bolivia",
                                                                "Colombia",
                                                                "Ecuador",
                                                                "Peru"), multiple = FALSE),
                       mainPanel(plotOutput("giican"))
                       )),
    
    # TAB 3: Gender Inequality Index 
    
    tabPanel("Women in Informal Work",
             fluidPage(titlePanel("Percentage over time"),
                       br(),
                       p(tags$em("Select an Andean Community Country")),
                       selectInput("countryInput", "Country", c("Bolivia",
                                                                "Colombia",
                                                                "Ecuador",
                                                                "Peru"), multiple = FALSE),
                       mainPanel(plotOutput("informalcan"))
             )),
    
    # TAB 4: Correlation
    
    tabPanel("Correlation",
             fluidPage(titlePanel("Linear Regression"),
                       br(),

                       mainPanel(plotOutput("correlation")),
                       
                       p("The correlation index between gender inequality and the percentage of women in informal work is 0.7.
                         That is a strong possitive correlation as showed in the graph."),
                       
                       h4(tags$b("Supporting women in the informal economy brings us closer to the 
                      goal of gender equity!"))
             )),
    
    # TAB 5: About 
    
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
                 
                 br(),
                 
                 p( "I am a Master in Design Engineering student, interested in capacity building for low-income workers. 
               This past year I have been working on the design of a development program, focused on creative capacity building 
               for women workers in the informal economy in Latin America. This data project aims to support the design of that
               program.
               
               For more information: dteran@mde.harvard.edu"),
                 
                 p(tags$a("https://github.com/danielate"))

))))


# Define server logic required to draw a histogram
server <- function(input, output) {

    # Load cover image of illustration I made. 
    
    output$cover <- renderImage({
        list(src = 'cover.png',
             height = 330,
             width = 500,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE)
   
    # Create first graph 

    output$giilatam <- renderPlot({
        
        gii_latam_long_s %>%
        ggplot(aes(
            x = country,
            y = gii,
            fill = (country %in% c("Bolivia", "Colombia", "Ecuador", "Peru"))
        )) +
            geom_col() +
            labs(
                title = "Gender Inequality Index of Latin American Countries",
                subtitle = "Year 2016",
                caption = "Source: United Nations Development Programme",
                x = "Country",
                y = "Gender Inequality Index (GII) ",
                fill = "Andean Community Countries") +
            theme_classic() +
            theme(legend.position = "right",
                  axis.text.x = element_text(angle = 45, hjust = 1)) 
    })
    
    # Create second graph 
    
    output$informallatam <- renderPlot({
        
        informal_latam_long_s %>%
            ggplot(aes(
                x = country,
                y = informality,
                fill = (country %in% c("Bolivia", "Colombia", "Ecuador", "Peru"))
            )) +
            geom_col() +
            labs(
                title = "Women Working in Informal Economy in Latin America",
                subtitle = "Year 2016",
                caption = "Source: International Labor Organization and World Bank",
                x = "Country",
                y = "Percentage",
                fill = "Andean Community Countries") +
            theme_classic() +
            theme(legend.position = "right",
                  axis.text.x = element_text(angle = 45, hjust = 1)) 
    })
    
    # Create third graph, reactive 
    
    output$giican <- renderPlot({
        
        filtered_gii <- gii_can_long_s %>%
            filter(country %in% input$countrycanInput)
    
            ggplot(data = filtered_gii, aes(x = Year, y = GII, group = country, color = country)) +
            geom_line() + 
            geom_point(size = 1) +
            theme_classic() + 
            labs(
                title = "Gender Inequality Index Over Time",
                subtitle = "Years 2011 to 2017",
                caption = "Source: United Nations Development Programme",
                x = "Country",
                y = "Gender Inequality Index (GII) ",
                color = "Country") 
    })
    
    # Create fourth graph, reactive 
    output$informalcan <- renderPlot({
        
        filtered_informal <- informal_can_long_s %>%
            filter(country %in% input$countryInput)
        
        ggplot(data = filtered_informal, aes(x = Year, y = Informality, group = country, color = country)) +
            geom_line() + 
            geom_point(size = 1) +
            theme_classic() + 
            labs(
                title = "Percentage of Women in Informal Work Over Time",
                subtitle = "Years 2011 to 2017",
                caption = "Source: International Labor Organization and World Bank",
                x = "Country",
                y = "Percentage",
                color = "Country") 
    })
    
    # Create fifth graph
    
    output$correlation <- renderPlot({
        
        join_can_s %>% 
        ggplot(aes(x = informality, y = gii)) +
            geom_point() +
            labs(x = "Percentage", y = "Gender Inequality Index",
                 title = "Relationship between Percentage of Women in Informal Work 
        and the Gender Inequality Index",
                 subtitle = "CAN Countries. 
       Years 2011 to 2017.",
                 caption = "Source: United Nations Development Programme. International Labor Organization and World Bank") +  
            geom_smooth(method = "lm", se = FALSE, color = "pink") +
            theme_classic() 
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
