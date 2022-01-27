
createLegend <- function(fp, df){
  # fp$legend.title = "Abandonment~reduction"
  # top <- parse(text = fp$legend.title, keep.source=FALSE)
  
  fp$legend.title = "N[2]*O~emissions \n (MgCO[2]*e)"
  topi <- parse(text = fp$legend.title, keep.source=FALSE)
  print('typeof(topi)')
  print(typeof(topi))
  # print('-----------------------------------------top----------------------------------------')
  
  # 
  fp$legend.subtitle = "Abandonment~reduction"
  bottom <- parse(text = fp$legend.subtitle, keep.source=FALSE)
  # print('-----------------------------------------bottom----------------------------------------')
  # print(bottom)
  # print(df)
  mapa <- df
  # legend <- createLegend(mapa, fp)
  
  ###get descriptive stats #########################################
  mapa$fill = cut(mapa$current_field, fp$bin_breaks)
  print(summary(mapa$current_field))
  meow = table(mapa$fill)
  
  # print('---------------meow----------------------------')
  # print(meow)
  
  ##This function turns a map into a dataframe that can more easily be plotted with ggplot2.
  mapa.df <- fortify(mapa)
  
  #fortify() creates zany attributes so need to reattach the values from intial dataframe
  #creates a numeric index for each row in dataframe
  mapa@data$id <- rownames(mapa@data)
  
  #merge the attributes of mapa@data to the fortified dataframe with id column
  mapa.df <- join(mapa.df, mapa@data, by="id")
  
  ##need to do this step sometimes for some dataframes or geometry looks "torn"
  # mapa.df <- mapa.df[order(mapa.df$order),]
  
  ##Use cut() function to divides a numeric vector into different ranges
  ##note: each bin must: 1)contain a value and 2)no records in dataframe can be null
  # mapa.df$fill = cut(mapa.df$current_field, fp$bin_breaks)
  print('-------------------------------   -(fp$sat) ----------------------------------------------------')
  print(-(fp$sat))
  
  ####saturation values #########################################
  mapa.df$col_sat = mapa.df$current_field
  mapa.df$col_sat[mapa.df$col_sat <= -(fp$sat)] <- -(fp$sat)
  mapa.df$col_sat[mapa.df$col_sat >= fp$sat] <- fp$sat
  
  
  print('-------------------------summary(mapa.df$col_sat)-------------------------------')
  print(summary(mapa.df$col_sat))
  print(mapa.df$col_sat)
  
  
  
  #### bring in shapefile for context in map ##################################
  states <- readOGR(dsn = "H:\\data\\cartography", layer = "states")
  
  #### bring in shapefile for context in map ##################################
  counties <- readOGR(dsn = "H:\\data\\cartography", layer = "counties")
  
  
  ###############################################
  #### graphics #################################
  ###############################################
  
  
  
  
  d = ggplot() +
    ### state boundary background ###########
  geom_polygon(
    data=states,
    aes(x=long,y=lat,group=group),
    # fill='#7e7e7e'
    # fill='#F5F5F5',
    fill='#cccccc'
  ) +
    
    ### county boundary strokes ###########
  geom_polygon(
    data=counties,
    aes(y=lat, x=long, group=group),
    fill=NA,
    # colour = '#cccccc',
    # colour='#808080',
    colour=NA,
    size=0
  ) +
    
    
    ### county choropleth map ###########
  geom_polygon(
    data=mapa.df,
    ###Aesthetic tells ggplot which variables are being mapped to the x axis, y axis,
    ###(and often other attributes of the graph, such as the color fill).
    ###Intuitively, the aesthetic can be thought of as what you are graphing.
    
    ###y-axis of graph referencing lat column
    ###x-axis of graph referencing long column
    ###group tells ggplot that the data has explicit groups
    ###fill color of features referencing fill column. Fill color is initially arbitrary (changing the color of fill will be addressed later in code)
    aes(y=lat, x=long, group=group, fill = col_sat),
    # colour = '#cccccc',
    # colour='#808080',
    colour=NA,
    size = 0
  ) +
    
    
    ### state boundary strokes ###########
  geom_polygon(
    data=states,
    aes(y=lat, x=long, group=group),
    # alpha=0,
    fill=NA,
    # colour='white',
    colour='#808080',
    size=1
  ) +
    
    
    
    
    
    ####NOTE: besure to set clip to off or the grob annotation is clipped by the dimensions of the panel
    coord_equal(clip = 'off') +
    
    #### add title to map #######
  labs(
    title = fp$plot.title,
    subtitle = fp$plot.subtitle
  ) +
    
    
    
    theme(
      #### nulled attributes ##################
      axis.text.x = element_blank(),
      axis.title.x=element_blank(),
      axis.text.y = element_blank(),
      axis.title.y=element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      
      panel.background = element_rect(fill = NA, color = NA),
      panel.grid.major = element_blank(),
      
      legend.text=element_text(size=25),
      legend.text.align = 0,
      legend.title.align = 1,
      legend.title=element_text(size=30),
      legend.box.just = "center",
      legend.key.width = unit(0.7,"cm"),
      legend.key.height = unit(2.5,"cm"),

      
      
      ###extend bottom margin of plot to accomidate legend and grob annotation
      plot.margin = unit(c(t=0, r=0, b=2, l=0), "cm"),
      
      #### modified attributes ########################
      ##parameters for the map title
      plot.title = element_text(size= 45, vjust=fp$plot.title.vjust, hjust=fp$plot.title.hjust, color = "#4e4d47", face="bold"),
      plot.subtitle = element_text(size= 40, vjust=fp$plot.subtitle.vjust, hjust=fp$plot.subtitle.hjust, color = "#4e4d47", face="bold")

    ) +
    
    scale_fill_gradientn(colors = rev(brewer.pal(11,"RdBu")),
                         values = rescale(c(-fp$sat, 0,fp$sat)),
                         limits=c(-fp$sat, fp$sat),
                         labels=comma,
                         # labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                         guide = guide_colourbar(title = parse(text=mapa@data$legend_title, keep.source=FALSE),
                                                 # direction = "vertical",
                                                 # title.vjust=1,
                                                 label.hjust=0,
                                                 title.position = 'top'
                         ))
  
  
  
  

  # ###align the legend title first using the entire map ggplot object
  c = align_legend(d, fp$align_legend.hjust)
  c

  return(c)

  
}





























































































