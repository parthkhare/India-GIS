#India-GIS
#=========

#GIS in R: Function Description

#==================================================================================
# Code: india_map_gui_ggplot.R 
# Purpose: generate heat maps for India
# Desc: creates categorical as well as continous maps for variables
# 
# Input(s): Provide 
#         - data set (at indian districts level)
#            ~ data set should have "IS_SC" for state and "IS_DC_11" for dsitrict  
#         - variable for which the heat map is to be generated
#            ~ data type of index variable should be factor/charcater
#            ~ data type of continous variable should be integer/numeric
#           - in case of continous variable do provide the number of cuts

# NOTE: the pop up can decide the variable on selection, if one wants to pass the
#       variable in the function then put in quotes

# # Sample function call
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# # GUI Pop up based controls
# # --------------
# Ind.gis(ds = dft,  cuts = 10)
# Ind.gis(ds = dft, var = , cuts = 10)
# 
# # Categorical Level
# # --------------
# Ind.gis(ds = dft, var = "som", cuts = 10)
# 
# # Continous Level
# # --------------
# Ind.gis(ds = dft, var = "exp_food", cuts = 10)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Auhor: parth
#==================================================================================

# MAP FUNCTION
#==================================================================================
Ind.gis <- function(ds, var=0, cuts)
{

# Schedule 1: Libraries
# ---------------  
  require(foreign)
  require(sp)
  require(maptools)  #tools for handling spatial objects
  require(RColorBrewer)
  require(dummies)
  require(kohonen)
  require(maps) #for creating geographical maps
  require(mapdata) #contains basic data for 'maps'
  require(mapproj) #for creating projected maps
  require(raster) #tools to deal with raster maps
  require(ggplot2) #to create maps

# Schedule 2: Variable name|location in the data set
# ---------------  
pars <- as.list(match.call()[-1])   # To call by variable name  

# Schedule 3: Gui pop up
# ---------------  
  interactive <- TRUE
  while (interactive == TRUE)
    {
      if (var == 0)
          {
            #show interactive window.
            color_by_var <- select.list(names(ds), multiple=FALSE,
                                        graphics=TRUE, 
                                        title="Choose var to color map by.")
            # check for user finished.
            if (color_by_var == "")
                { # if user presses Cancel - we quit function        
                return(TRUE)
                }
            interactive <- TRUE
          } 
      else
          {
            color_by_var <- as.character(pars$var)
            interactive <- FALSE
          }

# Variable locator: for main data 
# --------------
  for(i in 1: ncol(ds))
    {
      if(names(ds)[i] == color_by_var)
      {
        V1 <<- i
      }
    }

# Schedule 4: Map Data Import 
# ---------------
  map.wd <- "D:/Indicus/Projects/Maps/Shape Files/2011/AllIndiaSubDists_Districts_States/AllIndiaDistricts/"
  india <- readShapePoly(paste0(map.wd, "AllIndiaDistrictsV5")) # Districts

# Schedule 5: Merge with data set imported 
# ---------------
  india2 <- india
  india2@data <- merge(x = india2@data, y = ds, by.x = c("IS_SC","IS_DC"),
                       by.y = c("IS_SC","IS_DC_11"), all.x = T, sort =T)

# Schedule 6: Fortify
# -------------------------------
  ind_geom <- fortify(india2, region = "IS_DC")
  ind_geom <- merge(ind_geom, india2@data, by.x = "id", by.y = "IS_DC") # get attributes

# Variable locator for india2 shapefile
# --------------
  for(i in 1: ncol(ind_geom))
      {
        if(names(ind_geom)[i] == color_by_var)
        {
          V2 <<- i
        }
      }

# Schedule 6: Determine the type of map: cat or factor
# -------------------------------
  if(class(ds[,V1]) == "character"|class(ds[,V1]) == "factor"|class(ds[,V1]) == "logical")
        {
          # categorical variable: cap by the no of avaliable color pallettes
          # --------------------------------
          lvl <- length(levels(ind_geom[,V2]))
          lvl <- ifelse(lvl >= 12, 12, lvl)
          # Plot
          # ------------------
          map <- ggplot(ind_geom, aes_string("long","lat", group = "group", fill = color_by_var))
          map <- map + geom_polygon() + coord_equal() 
          map <- map + labs(x= "Lat", y = "Long", fill = paste0("Distribution of ", color_by_var))
          map <- map + scale_colour_discrete(brewer.pal(lvl,"Paired"), na.value = "grey 50")
          plot(map)
        }  
  else if(class(ds[,V1]) == "numeric"|class(ds[,V1]) == "integer")
        {
          # Create class intervals
          # ------------------
          set.seed(100)
          brks <<- quantile(ind_geom[,V2], seq(0,1,1/cuts), na.rm = T)          
          cols <<- colorRampPalette(brewer.pal(cuts,"Paired"))(cuts)
          colr <<- cols[findInterval(ind_geom[,V2], brks, all.inside=TRUE)]          
          # Plot
          # -----------------
          map <- ggplot(ind_geom, aes(long,lat, group = group, fill = factor(colr, labels = leglabs(round(brks)))))
          map <- map + geom_polygon() + coord_equal() 
          map <- map + labs(x= "Lat", y = "Long", fill = paste0("Distribution of ", color_by_var))
          plot(map)
        }
  }
 gc()
}
#===============================================================================


# Map function call
#===============================================================================
rm(list =ls())

# Data Import
# ----------------
load("D:/Indicus/Projects/Maps/Data/rur_som_dt.RData")
dim(dft)
dft$som <- as.factor(dft$som) # Convert the categorical class

# Call function
# -----------------
source("D:/Indicus/Projects/Maps/Codes/india_map_gui_ggplot.R")

# GUI Pop up based controls
# --------------
system.time(
Ind.gis(ds = dft,  cuts = 5)
)

Ind.gis(ds = dft, var = , cuts = 10)

# Categorical Level
# --------------
Ind.gis(ds = dft, var = "som", cuts = 10)
Ind.gis(ds = dft, var = "IS_SC", cuts = 10)
#===============================================================================
#FIN
