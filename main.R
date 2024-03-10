### สถิติเชิงอนุมาน การทดสอบสมมติฐานประชากรเดียว

data <- read.csv(file="data.csv")
View(data)
data1 <- as.numeric(data$Pay)

# Compute Z-value
xbar <- mean(data1)
mu <- 100
sigma <- sd(data1)
n <- length(data1)
z <- (xbar - mu) / (sigma / sqrt(n))

sprintf("xbar = %.2f", xbar)
sprintf("n = %d", n)
sprintf("sigma = %.2f", sigma)

# Compute Critical value
alpha <- .05
z.alpha <- qnorm(alpha)

sprintf("computed Z value : %.2f", z)
sprintf("critical Z value : %.2f", z.alpha)

# Compute p-value
p.value <- pnorm(z)
sprintf("p-value : %.2f" , p.value)

if(p.value >= alpha){
  print("Accept H0")
} else {
  print("Reject H0")
}
