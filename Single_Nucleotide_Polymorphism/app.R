#######
##Shiny App
#######
library(shiny)
library(rsconnect)
library(ggplot2)
library(dplyr)
snpData <- read.table("hg19_snp138_sub", header=T, sep="")
snpData$Group = as.character(sapply(snpData$chromosome, function(x) { 
   ifelse(x %in% c('X','Y'), 'A', ifelse(x == 'M', 'B', 'C'))}))

ui <- fluidPage(
   titlePanel("Human Single Nucleotide Polymorphisms"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("chr", "Chromosome", choices =c(1:22,'X','Y','M'), selected = 'X'),
         dateInput('date', label = 'Date of report'),
         radioButtons('group', label = 'Group of chromosomes', choices = c('A','B','C'),
                      selected = 'C')
      ),
      
      
      mainPanel(
         "Date report was generated:", verbatimTextOutput("dateText"),
         plotOutput("SPlot"), br(),
         "Number of SNPs in the above selected chromosome:", textOutput("Num"),
         plotOutput("GPlot"), br())
   )
)


server <- function(input, output){
   filtered <- reactive({
      snpData%>%
         filter(chromosome == input$chr)
   })
   
   output$SPlot <- renderPlot({
      ggplot(data = filtered()) + geom_jitter(mapping = aes(x=chromosome, y=position), size=0.25,  width = 0.25, color = 'maroon') + 
         theme(axis.title.x = element_text(color="blue", size=14, face="bold"),
               axis.title.y = element_text(color="darkgreen", size=14, face="bold"), 
               axis.text.x = element_text(face="bold", size=20))
   })
   
   # group <- reactive({input$group})
   filtered_group <- reactive({
      snpData%>%
         filter(Group == input$group)
   })
   output$GPlot <- renderPlot({
      ggplot(data = filtered_group(),  aes(x=chromosome, y=position, color = chromosome)) + geom_jitter(size=0.25,  width = 0.25) + 
         theme(axis.title.x = element_text(color="blue", size=14, face="bold"),
               axis.title.y = element_text(color="darkgreen", size=14, face="bold"), 
               axis.text.x = element_text(face="bold", size=15))
   })
   
   output$Num <- renderText({
      nrow(filtered())
   })
   
   output$dateText  <- renderText({
      as.character(input$date)})
   
   
}


shinyApp(ui = ui, server = server)

