  library(ggplot2)
  library(maps)
  library(rgdal)# R wrapper around GDAL/OGR
  library(sp)
  # require("RPostgreSQL")
  library(RPostgreSQL)
  library(postGIStools)
  library(plyr)
  library(dplyr)
  library(viridis)
  library(scales)
  library(rjson)
  # library(jsonlite)
  require(RColorBrewer)
  library(glue)
  # library(ggpubr)
  library(cowplot)
  library(stringi)
  library(gridBase)
  library(grid)
  library(gridSVG)
  library(gridExtra) #load Grid
  library(extrafont)
  library(sf)
  library(rasterVis)
  library(ggthemes) # theme_map()
  library(ggpmisc)
  
  library(stringr)
  library(mgsub)
  library(stringi)
  
  
  library("ggpubr")
  
  # load fonts - every session
  loadfonts(device = "win", quiet = TRUE)
  
  
  rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
  gc() #free up memrory and report the memory usage.
  
  #####kill the postgres connections each time run script
  killDbConnections <- function () {
  
    all_cons <- dbListConnections(PostgreSQL())
  
    print(all_cons)
  
    for(con in all_cons)
      +  dbDisconnect(con)
  
    print(paste(length(all_cons), " connections killed."))
  
  }
  
  killDbConnections()
  
  

  
  ###get postgres connections ############################
  con <- dbConnect(PostgreSQL(), dbname = 'synthesis', user = user, host = host, port=port, password = password)
  
  
  ##### link to scripts #####################################################
  rootpath = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\archived_projects\\synthesis\\graphics\\rcode'
  source(paste(rootpath, 'synthesis_maps.R', sep='\\'))
  source(paste(rootpath, 'synthesis_legend.R', sep='\\'))

  
  
  # ###### link to json files #################################################
  figure_json = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\archived_projects\\synthesis\\graphics\\figure_json.json'
  figure_obj<- fromJSON(file=figure_json)
  
  
  
  
  
  getQuery <- function(fp){
    
    if(fp$dataset %in% c('area_region_exp','area_region_abd','area_region_net')){
      query <- "SELECT 
                '' AS units_initial,
                '' AS units,
                '' AS conv_factor,
                'ds' AS data,
                t3.lrr_group as zone,
                sum(t1.mean) AS sum,
                sum(t2.acres_calc) AS qaqc_acres,
                st_area(st_union(st_snaptogrid(t2.geom, 0.0001::double precision))) * 0.00024710538146717::double precision AS acres,
                sum(t1.mean) / (st_area(st_union(st_snaptogrid(t2.geom, 0.0001::double precision))) * 0.00024710538146717::double precision) * 100::double precision * 1::double precision AS current_field,
                m.legend_title_supp as legend_title,
                st_union(st_snaptogrid(t2.geom, 0.0001::double precision)) AS geom
               FROM schema.input_ds t1
                 JOIN spatial.counties t2 USING (fips)
                 JOIN extensification_ksu.extensification_county_regions t3 USING (fips),
                 (SELECT legend_title_supp FROM misc.meta_extensification INNER JOIN misc.meta_title USING(indx_title)WHERE dataset = 'area_region') as m
              GROUP BY t3.lrr_group, m.legend_title_supp"
      query = mgsub::mgsub(query, c("schema", "ds", "input_ds"), c(fp$schema, fp$dataset, fp$input_dataset))
      return(query)
    }
      
    else{ 
      query <-
      "SELECT
      t1.fips as zone,
      t1.mean,
      (t1.mean * t3.conv_factor)*inversion_coeff  as current_field,
      t3.units_initial,
      t3.units,
      t3.conv_factor,
      t3.dataset as data,
      t3.legend_title,
      t2.geom
      
      FROM 
      schema.ds as t1
      INNER JOIN spatial.counties as t2 USING(fips),
      
      (
      SELECT
      m1.dataset,
      m2.units_initial,
      m2.units,
      m2.conv_factor,
      m3.legend_title_supp as legend_title
      FROM
      misc.meta_parent as m1 
      INNER JOIN
      misc.meta_cf as m2 USING(indx_cf)
      INNER JOIN
      misc.meta_title as m3 USING(indx_title)
      WHERE m1.dataset = 'lookup') as t3"
      query = mgsub::mgsub(query, c("ds", "lookup", "inversion_coeff", "schema", "parent"), c(fp$dataset, fp$lookup, fp$inversion_coeff, fp$schema, fp$parent))
      return(query)
    }
  }
  
  
  
  
  ###############################  create DataFrame  ##############################################################
  ## description: get the specific query for current dataset and create a dataframe from the query
  creatDF <- function(fp){
    
    query = getQuery(fp)
    ### structure the print layout so easy to read and transfer to postgres 
    print(cat(paste(strwrap(query, 40), collapse="\n")))
    df <- get_postgis_query(con, query, geom_name = "geom")
    return(df)
    
  }
  
  
  runMain <- function(figure_params){
  
    merge_ggplotlists <- list()
    # qaqc1 <- list()
    # qaqc2 <- list()
    
    
    ### create an empty dataframe
    meta.units <- data.frame(fig=character(), 
                             unit=character())
  
    for(i in figure_params$panels){
      print('start--------------------------------')
  
      fp = append(figure_params$core, i)
      # print('*********************** fp ********************************************')
      
      ##### create dataframe
      df <- creatDF(fp)
      
      #### export dataframe to csv ########################
      
      #### copy dataframe to manipulate ---- NOTE: be sure to use @data with the spatial dataframe or acts funny
      meta = df@data
      print(meta)
      
      #### select specific columns from dataframe
      meta.values = meta[c('zone', 'current_field')]
      
      #### change column headers ################
      colnames(meta.values) <- c('zone', fp$df2csv)
      
      csv_file = paste0('I:\\projects\\synthesis\\figures\\pnas\\',parent,'\\meta\\input\\',parent,'_',figure,'_',fp$dev.plot.title,'_',fp$df2csv,'.csv')
      write.csv(meta.values, csv_file, row.names=F)
      
      
      #### select specific columns from dataframe
      meta.units <- meta.units %>% add_row(fig = fp$df2csv, unit = as.factor(unique(meta[c('units')])))
      
      #######################################################
      
  
      #### create the ggplot map object
      ggplot_object <- createMap(fp, df)
  
      ##### create a list containing the ggplot map objects
      merge_ggplotlists <- append(merge_ggplotlists, list(ggplot_object))
    }
    
    #### export units metadata dataframe to csv file
    csv_meta.units = paste0('I:\\projects\\synthesis\\figures\\pnas\\',parent,'\\',fp$pnas_fig,'_units.csv')
    write.csv(meta.units, csv_meta.units, row.names=F)

    return(merge_ggplotlists)
  
  }
  
  
  align_legend <- function(p, hjust = hjust)
  {
    # extract legend
    g <- cowplot::plot_to_gtable(p)
    print('g')
    print(g)
    grobs <- g$grobs
    legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
    legend <- grobs[[legend_index]]
    
    # extract guides table
    guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
    
    # there can be multiple guides within one legend box  
    for (gi in guides_index) {
      guides <- legend$grobs[[gi]]
      
      # add extra column for spacing
      # guides$width[5] is the extra spacing from the end of the legend text
      # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
      # both sides, we get an aligned legend
      spacing <- guides$width[5]
      guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
      guides$widths[6] <- (1-hjust)*spacing
      title_index <- guides$layout$name == "title"
      guides$layout$l[title_index] <- 2
      
      # reconstruct guides and write back
      legend$grobs[[gi]] <- guides
    }
    
    # reconstruct legend and write back
    g$grobs[[legend_index]] <- legend
    return(legend)
  }
  
  
  
  runMain_legend <- function(figure_params, ss_array){
    
    panels = figure_params[["panels"]]
    panels=panels[ss_array]

    
    merge_ggplotlists <- list()
    qaqc1 <- list()
    qaqc2 <- list()
    
    for(i in panels){
      print('start--------------------------------')
      
      fp = append(figure_params$core, i)
      # print('*********************** fp ********************************************')
      
      ##### create dataframe
      df <- creatDF(fp)

      
      print('----------------------------df-------------------------------------------------')
      # print(df)

      
      
      #### create the ggplot map object
      ggplot_object <- createLegend(fp, df)

      ##### create a list containing the ggplot map objects
      merge_ggplotlists <- append(merge_ggplotlists, list(ggplot_object))
    }
    
    
    # print(qaqclist)
    return(merge_ggplotlists)
    
  }
  
  
  

  
  #################################################################################
  ################### central scripts #############################################
  #################################################################################
  
  pro <- function(){
  
    print('in production environment')
    ###DEFINE THE FIGURE OBJECT HERE!!!!!!!!!!!!!!!!!!!
    map_panels <- runMain(figure_params)
  
    lay <- matrix(c(rep(1:figure_params$core$dev.cnt, each=4)), nrow = ceiling(figure_params$core$dev.cnt/3), byrow = TRUE)
    g <- arrangeGrob(grobs = map_panels, layout_matrix = lay)
    
    
    
    fileout = paste0('I:\\projects\\synthesis\\figures\\',figure_params$core$output_dir,'\\',figure_params$core$output_file,'.png')
    ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500, g)
    
  }
  
  
  

  
  #####################################################################
  ######## run script #################################################
  #####################################################################
  
  ######### figure parameters ###############################
  parent="extensification"
  figure="fig2"


  figure_params = figure_obj[[parent]][[figure]]
  # print(figure_params)
  core = figure_params[["core"]]
  
  
  
  
  ##############################################################################################
  ############################################################################################
  

  
  
  
  
  
  ##### intensification fig3 ##############################################################################
  if(parent == 'intensification' && figure == 'fig3'){
    map_panels <- runMain(figure_params)
    legend_panels <- runMain_legend(figure_params, ss_array=c("p1","p4"))

    row1 = ggarrange(map_panels[[1]], map_panels[[2]], map_panels[[3]], ncol = 3, nrow = 1)
    row2 = ggarrange(map_panels[[4]], map_panels[[5]], map_panels[[6]], ncol = 3, nrow = 1)

    map = ggarrange(row1, row2, ncol = 1, nrow = 2, align = 'hv')
    legend = ggarrange(legend_panels[[1]], legend_panels[[2]], ncol = 1, nrow = 2, align = 'hv')


    final = ggarrange(map,legend, ncol = 2, nrow = 1, widths = c(4,1), align = 'hv')
    fileout = paste0('I:\\projects\\synthesis\\figures\\pnas\\',parent,'\\',figure_params$core$pnas_fig,'.png')
    ggplot2::ggsave(fileout, width = 40, height = 25, dpi = 500)
  }else{
    map_panels <- runMain(figure_params)
    legend_panels <- runMain_legend(figure_params, ss_array=c("p1","p4","p7"))
    
    row1 = ggarrange(map_panels[[1]], map_panels[[2]], map_panels[[3]], ncol = 3, nrow = 1)
    row2 = ggarrange(map_panels[[4]], map_panels[[5]], map_panels[[6]], ncol = 3, nrow = 1)
    row3 = ggarrange(map_panels[[7]], map_panels[[8]], map_panels[[9]], ncol = 3, nrow = 1)
    
    map = ggarrange(row1, row2, row3, ncol = 1, nrow = 3, align = 'hv')
    legend = ggarrange(legend_panels[[1]], legend_panels[[2]],legend_panels[[3]], ncol = 1, nrow = 3, align = 'hv')
    
    
    final = ggarrange(map,legend, ncol = 2, nrow = 1, widths = c(4,1), align = 'hv')
    
    fileout = paste0('I:\\projects\\synthesis\\figures\\pnas\\',parent,'\\',figure_params$core$pnas_fig,'.png')
    ggplot2::ggsave(fileout, width = 40, height = 25, dpi = 500)
  }
 


  
  

  
  
  
  
