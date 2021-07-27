
#profile data #1
y <- c(5, 15, 32, 55, 100, 200)
x <- c(5, 4, 3, 2.2, 1.7, 0.5)

#Example data #2
y <- c(5, 15, 45, 100)
x <- c(5, 4, 2.6, 1.7)


# make spline fit
smoothingSpline = smooth.spline(y, x, spar=0.45)

#Approximate value at 30 cm
SOCval <- predict(smoothingSpline, 30)

#plot it all
target_val = 30
plot(y, x, 
     main="Soil Profile SOC Smooth Fit Example",
     ylab="SOC Concentration", 
     xlab="Soil Depth")
lines(smoothingSpline, col="blue")
abline(v = target_val, col="red", lwd=1, lty=2)
abline(h = SOCval, col="red", lwd=1, lty=2)
text(55, 4, paste0("At ", target_val, " cm, the SOC stock value is ",round(SOCval$y,2)))




