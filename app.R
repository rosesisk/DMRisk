#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Diabetes Risk Calculators: Cardiovascular Events"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
           numericInput("age",
                        "Age:",
                        min = 16,
                        max = 105,
                        value = 55),
        radioButtons("gender",
                     "Gender:",
                     choices = list("Female" = 1, "Male" = 0)),
       
         numericInput("diabdur",
                     "Diabetes duration (years):",
                     min = 0,
                     max = 105,
                     value = 10),
        numericInput("sysbp",
                     "Systolic BP (mmHg)",
                     min = 70,
                     max = 270,
                     value = 120),
        numericInput("ldl",
                     "LDL Cholesterol (mmol/l)",
                     min = 0,
                     max = 50,
                     value = 2.5),
        numericInput("tchdl",
                     "Total Cholesterol/HDL Ratio",
                     min = 0,
                     max = 100,
                     value = 4),
        numericInput("hba1c",
                     "HbA1c (mmol/mol)",
                     min = 0,
                     max = 500,
                     value = 45),
        radioButtons("microalb",
                     "Microalbuminuria",
                     choices = list("No" = 0, "Yes" = 1)),
        radioButtons("macroalb",
                     "Macroalbuminuria",
                     choices = list("No" = 0, "Yes" = 1)),
        numericInput("egfr",
                     "eGFR (ml/min/1.73m^2)",
                     min = 0,
                     max = 500,
                     value = 90),
        radioButtons("smoke",
                     "Do you smoke?",
                     choices = list("Yes" = 1, "No" = 0)),
        radioButtons("exer",
                     "Do you exercise regularly?",
                     choices = list("Yes" = 0, "No" = 1)),
        radioButtons("ac",
                     "Ethnicity",
                     choices = list("Afro-Carribean" = 1, "Caucasian or Asian-Indian" = 0))),
        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("overlay"),
           textOutput("risk10steno"),
          br(),
            textOutput("risk10ukpds")))
           
        )
    
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$overlay <- renderPlotly({
        # generate bins based on input$bins from ui.R
      age_ind_lt40 <- ifelse(input$age < 40, 1, 0)
      age_ind_gte40 <-  ifelse(input$age >= 40, 1, 0)
      
       t <- seq(0, 15, 0.01)
       r <- exp(-6.046429053 + (0.040672727 * input$age) + (-0.234111177*as.numeric(input$gender)) +
           (0.013062752*input$diabdur) + (0.005814221*input$sysbp) + (0.082287009*input$ldl) + (0.012209026*input$hba1c) + (0.437359313*as.numeric(input$microalb)) +
           (0.738916137*as.numeric(input$macroalb)) + (-0.404318528*log2(input$egfr)*age_ind_lt40)+ (-0.345596046*log2(input$egfr)*age_ind_gte40)+
           0.204224209*as.numeric(input$smoke) + 0.229279688*as.numeric(input$exer))
       
       age_diag <- input$age - input$diabdur
       hba1c <- input$hba1c * 0.0915 + 2.15
       
       q <- 0.0112*(1.059^(age_diag - 55))*
         (0.525^as.numeric(input$gender))*(0.390^as.numeric(input$ac))*
         (1.350^as.numeric(input$smoke))*(1.183^(hba1c - 6.72))*
         (1.088^((input$sysbp - 135.7)/10))*(3.845^(log(input$tchdl)-1.59))
       
       R_t_steno <- 1-exp(-r*t)
       
       R_t_diag_ukpds <- 1-exp((1.078^input$diabdur)*(-q*(1-(1.078^t)))/(1-1.078))
       
       df <- tibble(Time = t, Steno = R_t_steno, UKPDS = R_t_diag_ukpds) %>%
         pivot_longer(-Time, names_to = "algorithm", values_to = "risk") 
       
      p <- ggplot(data = df, aes(x = Time, y = risk, colour = algorithm, group = 1,
                                 text = paste("Estimated risk of CVD event within the next ", round(Time, 1), " years = ", round(risk, 3)*100, "%"))) +
          geom_line(size = 2) +
          xlab("Time in years") +
          ylab("Probability of experiencing a CVD event") +
          scale_colour_manual(name = "Algorithm", labels = c("Steno (Type 1)", "UKPDS (Type 2)"), values = c("steelblue4", "firebrick")) +
        ylim(0, 1)
      
      print(ggplotly(p, tooltip = "text", mode = "lines+markers"))
       
        # draw the histogram with the specified number of bins
        
    })
    output$risk10steno <- renderText({
        r <- exp(-6.046429053 + (0.040672727 * input$age) + (-0.234111177*as.numeric(input$gender)) +
                     (0.013062752*input$diabdur) + (0.005814221*input$sysbp) + (0.082287009*input$ldl) + (0.012209026*input$hba1c) + (0.437359313*as.numeric(input$microalb)) +
                     (0.738916137*as.numeric(input$macroalb)) + (-0.404318528*log2(input$egfr))+
                     0.204224209*as.numeric(input$smoke) + 0.229279688*as.numeric(input$exer))
        R_10 <- 1-exp(-r*10)
        paste0("The Steno algorithm estimates your risk of experiencing a CVD event over the next 10 years to be: ", round(R_10,3)*100, "%.", " This algorithm is designed for use in Type 1 diabetics, and uses: Age, Sex, LDL cholesterol, Diabetes duration, Systolic BP, HbA1c, micro & macroalbuminuria, eGFR, smoking and exercise to predict your risk.")
        
    })
    output$risk10ukpds <- renderText({age_diag <- input$age - input$diabdur
    hba1c <- input$hba1c * 0.0915 + 2.15
    
    q <- 0.0112*(1.059^(age_diag - 55))*
      (0.525^as.numeric(input$gender))*(0.390^as.numeric(input$ac))*
      (1.350^as.numeric(input$smoke))*(1.183^(hba1c - 6.72))*
      (1.088^((input$sysbp - 135.7)/10))*(3.845^(log(input$tchdl)-1.59))
    
    R_10 <- 1-exp((1.078^input$diabdur)*(-q*(1-(1.078^10)))/(1-1.078))
    
    paste0("The UKPDS algorithm estimates your risk of experiencing a CVD event over the next 10 years to be: ", round(R_10,3)*100, "%.", "This algorithm is designed for use in Type 2 diabetics, and uses: Age, Sex, Ethnicity, smoking status, HbA1c, Systolic BP and Total/HDL cholesterol ratio to predict your risk.")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
