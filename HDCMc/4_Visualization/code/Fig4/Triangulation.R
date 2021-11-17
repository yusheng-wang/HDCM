source("./R/PSTVB_Packages.R")
# data("GeoMap", package = "stBase")
# data("SiteData", package = "stBase")
###############################################################################
#                          construct triangles
###############################################################################
{
  grid <- CreateGrid(Site,
                     max.edge = c(.35, .65), #0.3,0.7
                     offset = c(1e-1, 0.5), #0.4, 0.6
                     cutoff = .05, #0.1
                     col = "black", size = 1)
  grid$plot.grid
  # https://haakonbakka.bitbucket.io/btopic103.html
  tl = length(grid$mesh$graph$tv[,1])
  # - the number of triangles in the mesh
  posTri = matrix(0, tl, 2)
  for (t in 1:tl){
    temp = grid$mesh$loc[grid$mesh$graph$tv[t, ], ]
    posTri[t,] = colMeans(temp)[c(1,2)] 
  }
  posTri = SpatialPoints(posTri)
  
  
  da <- Site
  coordinates(da) = ~ LON + LAT
}
size = c(15, 13, 4.0)
# size = c(5, 7, 1)
###############################################################################
#                                    plot
###############################################################################
###############################################################################
{
  p <- ggplot() + gg(grid$mesh, int.color = "transparent") + coord_equal()+ 
    geom_polygon(data = Map_BTH,
                 aes(x = long,y = lat, group = group),
                 colour = 'gray25', size = 0.5,
                 fill = NA)+
    scale_x_continuous(limits=c(112.5, 120.5),
                       breaks = seq(113, 120.5, 1.5), 
                       labels = paste0(seq(113, 120.5, 1.5), "° E")) +
    scale_y_continuous(limits=c(35.3, 43.3),
                       breaks = seq(35, 43.5, 2),
                       labels = paste0(seq(35, 43.2, 2), "° N"))+
    geom_text(aes(x = 119.5, y = 37
                  , label =  paste("Vertices: ", grid$mesh$n, " \n",
                                   "Triangles: ", 
                                   length(grid$mesh$graph$tv[,1]))),
              color = "black", size = size[3]) +
    geom_point(data = Site, aes(x = LON, y = LAT)
               , shape = 20
               , col = "red", size = 1) +
    theme_bw() + 
    labs(x =  "longitude", y = "latitude") +
    theme( axis.ticks = element_line(size = 1, colour = "black")
           , axis.text = element_text(size = size[2], colour = "black")#, face = "bold"
           , axis.title= element_text(size = size[1], colour = "black")
           , legend.title = element_blank()
           , legend.position = "none"
           , panel.grid.major = element_blank()
           , panel.grid.minor = element_blank()
    )
}
# ggsave(plot = p, height = 5, dpi = 300, file = './figure/Fig4.png')
###############################################################################
ggsave(plot = p, height = 6, file = './figure/Fig4.pdf')
###############################################################################
