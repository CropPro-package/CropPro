#Testing

crop.plot2D(cropproc, Func1 = 1, Func2 = 3)
crop.plot2D(cropproc, Func1 = 3, Func2 = 1)
crop.plot2D(cropproc, Func1 = 2, Func2 = 1)

crop.dung_plot2D(dungproc, Func1 = 2, Func2 = 3)
crop.dung_plot2D(dungproc, Func1 = 3, Func2 = 2)
crop.dung_plot2D(dungproc, Func1 = 1, Func2 = 3)
dung.plot2d(dungproc, Func1 = 3, Func2 = 1)
dung.plot2d(dungproc, Func1 = 2, Func2 = 1)
dung.plot2d(dungproc, Func1 = 1, Func2 = 2)
dung.plot2d(dungproc, Func1 = 3, Func2 = 4)
dung.plot2d(dungproc, Func1 = 2, Func2 = 4)
dung.plot2d(dungproc, Func1 = 1, Func2 = 4)
dung.plot2d(dungproc, Func1 = 4, Func2 = 2)
dung.plot2d(dungproc, Func1 = 4, Func2 = 3)
dung.plot2d(dungproc, Func1 = 4, Func2 = 1)


#graphs for presentation
dungproc<-LDAcrop.dung(datatrans)

results<-cbind(cropproc$Samples ,cropproc$`Class_std*`, dungproc$`CLASS_std*`, dungproc$Samples)
colnames(results)<-c("Samples","crop_Class", "dung_Class", "samples")
play3d( spin3d( axis = c(0, 0, 1), rpm = 10), duration = 20 )
library(magick)
movie3d(
  movie="3dAnimatedScatterplot",
  spin3d( axis = c(0, 0, 1), rpm = 6),
  duration = 20,
  dir = "~/Desktop",
  type = "gif",
  clean = TRUE
)
dungproc<-LDAcrop.dung(datatrans)

crop.plot2D(cropproc, Func1 = 2, Func2 = 3)


crop.dung_plot2D(dungproc, site = "Stafford", col = "red")


crop.dung_plot3D(dungproc, site= "Stafford")
library(magick)
movie3d(
  movie="3dAnimatedScatterplot_dung",
  spin3d( axis = c(0, 0, 1), rpm = 6),
  duration = 20,
  dir = "~/Desktop",
  type = "gif",
  clean = TRUE
)
crop.dung_plot2D(dungproc, site = "Stafford", Func1 = 2, Func2 = 3)
