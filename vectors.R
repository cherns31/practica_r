
# Dot product of ortogonal vectors, i.e. with a 90 deg angle, is 0

p1 = c(3, 1)
p2 = c(1,-3)

(den <- norm(as.matrix(p1), type = '2')*norm(as.matrix(p2), type = '2'))
(num <- p1%*%p2 )


rad2deg(acos(num/den))     

plot(rbind( p1,p2, -p1,-p2 ))
segments(-p1[1], -p1[2], x1 = p1[1], y1 = p1[2])
segments(-p2[1], -p2[2], x1 = p2[1], y1 = p2[2])


# Dot product is length of vectors * cos of their angle; a · b = |a| × |b| × cos(θ) 
norm(as.matrix(p1), type = '2')*norm(as.matrix(p2), type = '2')*round(cos(pi/2), 2) # Here the cos is 0 so the length doesn't matter
num == round(norm(as.matrix(p1), type = '2')*norm(as.matrix(p2), type = '2')*cos(pi/2), 2)



# Let's try with a 45 deg, and find the required vector if another is given

rad2deg(pi/4)
cosine = cos(pi/4)

p1 = c(3, 1)  
p1_norm = norm(as.matrix(p1), type = '2')
ref = p1_norm*cosine


# c(3,1)*b =  2.236 * |b|
# |b| / b = p1 /  2.236


# Si |b| = 1

(ref/3)**2 - 1**2
# p1_norm*cosine*1 = 3*p21 + 1*p22. 
# y sqrt(p21*p21 + p22*p22) = 1

# p21 = ((p1_norm*cosine*p2_norm)-1*p22)/3
# Entonces sqrt([((p1_norm*cosine*p2_norm)-1*p22)/3]**2 + p22*p22) = 1
# Entonces sqrt([(p1_norm*cosine - 1*p22)/3]**2 + p22*p22) = 1
# (sqrt(((p1_norm*cosine)/3-p22/3)**2 + p22**2) = 1
# (1/9 p22**2 - 0.496904 p22 - 0.444444 ) + p22**2 = 1**2
# 10/9 p22**2 - 0.496904 p22 - 0.444444  = 1

p22 = -0.447214
p21 = ((p1_norm*cosine*1)-1*p22)/3 # O al reves

p22_bis = 0.894427
p21_bis = ((p1_norm*cosine*1)-1*p22_bis)/3 # O al reves


p2 = c(p21,p22)
p2_bis = c(p21_bis,p22_bis)



(p2_norm = norm(as.matrix(p2), type = '2'))
(p2_bis_norm = norm(as.matrix(p2_bis), type = '2'))
round(p1_norm*cosine*p2_norm,5) == round(p1%*%p2,5)


plot(rbind( p1,p2, -p1,-p2 ))
segments(-p1[1], -p1[2], x1 = p1[1], y1 = p1[2])
segments(-p2[1], -p2[2], x1 = p2[1], y1 = p2[2])

plot(rbind( p1,p2_bis, -p1,-p2_bis ))
segments(-p1[1], -p1[2], x1 = p1[1], y1 = p1[2])
segments(-p2_bis[1], -p2_bis[2], x1 = p2_bis[1], y1 = p2_bis[2])


(den <- norm(as.matrix(p1), type = '2')*norm(as.matrix(p2), type = '2'))
(num <- p1%*%p2 )
rad2deg(acos(num/den))



(den2 <- norm(as.matrix(p1), type = '2')*norm(as.matrix(p2_bis), type = '2'))
(num2 <- p1%*%p2_bis )
rad2deg(acos(num2/den2))


# Defino funciones
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

angle <- function(x,y){
  dot.prod <- x%*%y 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(rad2deg(theta))
}


result <- function(a,b,c){
  if(delta(a,b,c) > 0){ # first case D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    result = c(x_1,x_2)
    return (result)
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    x = -b/(2*a)
    return (x)
  }
  else {"There are no real roots."} # third case D<0
}

delta<-function(a,b,c){
  b^2-4*a*c
}


angle(p1, p2)
angle(p1, p2_bis)




# Si ahora generalizamos

# p1_norm*cosine*p2_norm = p11*p21 + p12*p22. 
# y sqrt(p21*p21 + p22*p22) = p2_norm
# p21 = ((p1_norm*cosine*p2_norm)-p12*p22)/p11
# Entonces sqrt([((p1_norm*cosine*p2_norm)-p12*p22)/p11]**2 + p22**2) = p2_norm
# [(p1_norm*cosine*p2_norm/p11) - (p12/p11)*p22)]**2 + p22*p22 = p2_norm**2
# [(-p12/p11)**2 * p22**2 +  2 * (-p12/p11) * (p1_norm*cosine*p2_norm/p11) * p  + [p1_norm*cosine*p2_norm/p11]**2 ) + p22**2 = p2_norm**2
# [(1+(-p12/p11)**2) * p22**2 +  2 * (-p12/p11) * (p1_norm*cosine*p2_norm/p11) * p  + ([p1_norm*cosine*p2_norm/p11]**2 - p2_norm**2 ) = 


ang = 75
cosine = cos(deg2rad(ang))

p1 = as.matrix(c(2,2))
p1_norm = norm(p1, type = '2') 
p2_norm = p1_norm # Make them the same length



a = (-p1[2,]/p1[1,])**2 + 1
b = 2 * (-p1[2,]/p1[1,])  * (p1_norm * cosine * p2_norm/p1[1,])
c = (p1_norm * cosine * p2_norm / p1[1,]) ** 2 - p2_norm**2




p22 = result(a,b,c)[1]
p21 = ((p1_norm*cosine*p2_norm)-p1[2,]*p22)/p1[1,] # O al reves

p22_bis = result(a,b,c)[2]
p21_bis = ((p1_norm*cosine*p2_norm)-p1[2,]*p22_bis)/p1[1,] # O al reves

# Chequeo si la norma de p2 quedo bien
sqrt(p22**2 + p21**2)
sqrt(p22_bis**2 + p21_bis**2)
(norm(as.matrix(p2), type = '2'))


# Armo el punto
(p2 = matrix(c(p21,p22),nrow = 2))
(p2_bis = matrix(c(p21_bis,p22_bis),nrow = 2))



round(p1_norm*cosine*p2_norm,5) == round(t(p1)%*%p2, 5)
round(p1_norm*cosine*p2_norm,5) == round(t(p1)%*%p2_bis, 5)



plot(rbind(c(0,0), t(p1), t(p2), t(p2_bis)))
segments(0, 0, x1 = p1[1], y1 = p1[2], col = "black", lwd = 2)
segments(0, 0, x1 = p2[1], y1 = p2[2], col = "blue", lwd = 2)
segments(0, 0, x1 = p2_bis[1], y1 = p2_bis[2], col = "blue", lwd = 2)

angle(t(p1), p2)
angle(t(p1), p2_bis)
cos(deg2rad(angle(t(p1), p2_bis)))

p1_norm
p2_norm

p1
p2
p2_bis


angle(t(p2), p2_bis)
