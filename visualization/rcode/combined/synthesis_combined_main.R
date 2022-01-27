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


#### figure parameters ####################
parent="combined"
figure="fig1"


###get postgres connections ############################
con <- dbConnect(PostgreSQL(), dbname = 'synthesis', user = user, host = host, port=port, password = password)




##### link to scripts #####################################################
rootpath = 'C:\\Users\\Bougie\\Desktop\\scripts\\projects\\archived_projects\\synthesis\\graphics\\rcode\\combined'
source(paste(rootpath, 'synthesis_combined_maps.R', sep='\\'))



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
                t3.lrr_group,
                sum(t1.mean) AS sum,
                sum(t2.acres_calc) AS qaqc_acres,
               ---st_area(st_union(st_snaptogrid(t2.geom, 0.0001::double precision))) * 0.00024710538146717::double precision AS acres,
                st_area(st_union(st_snaptogrid(t2.geom, 0.0001::double precision))) * 0.0001::double precision AS acres,
                ---sum(t1.mean) / (st_area(st_union(st_snaptogrid(t2.geom, 0.0001::double precision))) * 0.00024710538146717::double precision) * 100::double precision * 1::double precision AS current_field,
                sum(t1.mean) / (st_area(st_union(st_snaptogrid(t2.geom, 0.0001::double precision))) * 0.0001::double precision) * 100::double precision * 1::double precision AS current_field,
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
    m3.legend_title_main as legend_title
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
  qaqc1 <- list()
  qaqc2 <- list()
  
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
    print(csv_file)
    write.csv(meta.values, csv_file, row.names=F)
    
    
    ############### create units metadata table #######################################################
    
    #### select specific columns from dataframe
    meta.units <- meta.units %>% add_row(fig = fp$df2csv, unit = as.factor(unique(meta[c('units')])))

    
    print('----------------------------df-------------------------------------------------')
    # print(df)
    #### create qaqc object 
    qaqc1 <- rbind(qaqc1, qaqc(df, fp)[[1]])
    print(qaqc1)
    qaqc2 <- rbind(qaqc2, qaqc(df, fp)[[2]])
    print(qaqc2)
    

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







##################################################################################
######## qaqc table to check dataframe creation and get the ranges of scatterplots ###############################
#############################################################################################
qaqc <- function(df, fp){
  print('qaqc function___________________________________')

  test <- df@data

  print('summary-----------------  line 211 -----------------')


  qaqc_1 <- data.frame(plot_title=fp$plot.title,
                        schema=fp$schema,
                        dataset=fp$dataset,
                        units_initial=test %>% distinct(units_initial),
                        units=test %>% distinct(units),
                        conv_factor=test %>% distinct(conv_factor),
                        legend_title=test %>% distinct(legend_title))

  test <- test$current_field
  print(summary(test))
  qaqc_2 <- data.frame(dataset=fp$plot.title, min=min(test), q1=as.numeric(quantile(test,0.25)), q2=as.numeric(quantile(test,0.5)), mean=mean(test),q3=as.numeric(quantile(test,0.75)), max=max(test), sat=paste(fp$sat, collapse=', '))
  
  qaqc_list <- list(qaqc_1, qaqc_2)
  
  
  return(qaqc_list)
}


createRangeMaps <- function(df, fp){
  
 print('---------------------- createRangeMaps ----------------------------------------------')
  # fp$dev.krnl = as.numeric(quantile(df@data$current_field,fp$dev.q))
  # print('fp$dev.krnl')
  # print(fp$dev.krnl)
  #### create increment list for neg/pos values and then bind together
  incr_list = sapply(c(1:fp$dev.cnt), function(x) {(x * fp$dev.incr) + fp$dev.krnl})
  # result = data.frame(cbind(-incr_list, incr_list))
  result = data.frame(incr_list)
  print('----------------------------------results--------------------------------------------------------------')
  print(result)
  map_panels <- list()
  
  i = 1
  while(i<=fp$dev.cnt){
    
    # print(as.numeric(result[i,]))
    fp$plot.title=i
    
    print('--------------------------incr_list[i]----------------------------------------------------------------------')
    print(incr_list[i])
    fp$sat=incr_list[i]
    # fp$sat=as.numeric(result[i,])

    #### create the ggplot map object
    ggplot_object <- createMap(fp, df)

    map_panels <- append(map_panels, list(ggplot_object))
    
    i=i+1
  }

  
  lay <- matrix(c(rep(1:fp$dev.cnt, each=4)), nrow = ceiling(fp$dev.cnt/3), byrow = TRUE)


  g <- arrangeGrob(grobs = map_panels, layout_matrix = lay)
  fileout = paste0('I:\\projects\\synthesis\\qaqc\\figures\\',
                   fp$output_dir,'\\',
                   fp$output_file,'\\',
                   fp$output_file,'_',fp$dev.plot.title,'.png')
  print(fileout)
  ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500, g)
  

}


#################################################################################
################### central scripts #############################################
#################################################################################

pro <- function(){

  print('in production environment')
  ###DEFINE THE FIGURE OBJECT HERE!!!!!!!!!!!!!!!!!!!
  map_panels <- runMain(figure_params)
  
  
  # print(figure_params$core$dev.cnt)

  lay <- matrix(c(rep(1:figure_params$core$dev.cnt, each=4)), nrow = ceiling(figure_params$core$dev.cnt/3), byrow = TRUE)
  g <- arrangeGrob(grobs = map_panels, layout_matrix = lay)
  
  
  
  fileout = paste0('I:\\projects\\synthesis\\figures\\',figure_params$core$output_dir,'\\',figure_params$core$output_file,'.png')
  ggplot2::ggsave(fileout, width = 34, height = 25, dpi = 500, g)
  
}



 
#####################################################################
######## run script #################################################
#####################################################################



figure_params = figure_obj[[parent]][[figure]]
core = figure_params[["core"]]


##### production #################################################

print('in production environment')
### define figure object here 
map_panels <- runMain(figure_params)

lay <- matrix(c(rep(1:figure_params$core$dev.cnt, each=4)), nrow = ceiling(figure_params$core$dev.cnt/3), byrow = TRUE)
g <- arrangeGrob(grobs = map_panels, layout_matrix = lay)


fileout = paste0('I:\\projects\\synthesis\\figures\\pnas\\',parent,'\\',figure_params$core$pnas_fig,'.png')
ggplot2::ggsave(fileout, width = 30, height = 25, dpi = 500, g)










