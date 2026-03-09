df <- read.table("eg_uv_vis.txt")
colnames(df)[1] <- "nm"

legend <- T

uv.plot(df,save = F,legend = legend)

# NEED TO MAKE IT SO THAT FIRST COLUMN DOES NOT HAVE TO BE NAMED "NM"
