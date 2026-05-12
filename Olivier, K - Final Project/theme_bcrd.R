empalme <- function(x,y){
  x <- as.data.frame(x)
  y <- as.integer(y)
  nam <- names(x)
  
  if(nrow(x) >= y) {return(x)} else { z1 <- matrix(NA, nrow = as.numeric(y-nrow(x)), ncol = ncol(x))
  z2 <- as.data.frame(z1)
  names(z2) <- nam
  z <- rbind(x, z2) 
  return(z)}
}
var_pct <- function(x,y=12) {
  x1 <- as.vector(x)
  dx1 <- dplyr::lag(x1, as.numeric(y))
  z <- 100*(x1 - dx1)/dx1
  
  z
  
}
empalme_fecha <- function(x,y){
  x <- as.Date(x)
  y <- as.data.frame(y)
  nam <- names(x)
  
  if(nrow(x) >= y) {return(x)} else { z1 <- matrix(NA, nrow = as.numeric(y-nrow(x)), ncol = ncol(x))
  z2 <- as.data.frame(z1)
  names(z2) <- nam
  z <- rbind(x, z2) 
  return(z)}
}

force_bind = function(df1, df2) {
  colnames(df2) = colnames(df1)
  bind_rows(df1, df2)
} #ESTA FUNCION HACE UN RBIND MANTENIENDO EL NOMBRE DE LAS COLUMNAS


theme_bcrd <- theme(plot.title = element_text(family = "Gotham Book", face = "bold", size = (13), hjust = 0.5),
                    plot.subtitle = element_text(family = "Gotham Book" ,hjust = 0.5, size = 10),
                    plot.caption = element_text(hjust = -0.05),
                    legend.title = element_blank(),
                    legend.text = element_text(face = "plain", colour="black",family = "Gotham Book", size = (10)),
                    axis.title = element_blank(),
                    axis.text = element_text(family = "Gotham Book", colour = "black", size = (10)),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black") ,
                    legend.position = c(.20, 0.975),
                    legend.justification = c("center", "top"),
                    legend.box.just = "center",
                    legend.margin = margin(3, 3, 3, 3),
                    legend.box.background = element_blank(),
                    legend.key = element_blank(),
                    legend.box = "horizontal",
                    legend.key.size = unit(0.5, "cm"),
                    axis.text.x = element_text(face = "plain", colour="black",family = "Gotham Book", size = (10)),
                    axis.text.y = element_text(face = "plain", colour="black",family = "Gotham Book", size = (10)))
