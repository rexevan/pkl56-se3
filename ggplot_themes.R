library(ggplot2)
library(grid)
library(RColorBrewer)

# a BIG thanks to Max Woolf for making this theme
# Source : http://minimaxir.com/2015/02/ggplot-tutorial/

theme_fte <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.legend.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=12) +
    theme(text = element_text(family = "iosevka")) + 
    theme(plot.title = element_text(family = "Liberation Sans",  size = 20)) + 
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.25)) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=11,color=color.axis.title)) +
    theme(legend.key = element_blank()) + 
    theme(legend.position = "top") + 
    theme(legend.title = element_text(color = color.legend.title)) +
    theme(legend.justification = c(0,1)) +
    
    # Format the Facet/Strip 
    theme(strip.background =  element_blank()) +
    theme(strip.text = element_text(size = 12, face = "bold")) + 

    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(size=15, vjust=1.25, color = "#3E3E3E", face = "bold")) +
    theme(plot.subtitle = element_text(face = "bold", color = "#3E3E3E")) + 
    theme(plot.caption = element_text(color = "#3E3E3E", face = "italic")) + 
    theme(axis.text.x=element_text(size=12,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=12,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=13,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=13,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
