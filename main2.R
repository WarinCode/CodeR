### สถิติเชิงอนุมาน การทดสอบค่าสัดส่วนของประชากรเดียว

data <- read.csv(file="data.csv")
View(data)

# Compute z-value
# นิสิตที่ชอบรับประทานข้าวจำนวน 30 คน
n_rice <- length(data$Deliciousness[data$Deliciousness == "rice"])
# นิสิตที่ชอบรับประข้าวห้อมมะลิจำนวน 24 คน
n_thia_jasmine_rice <- length(data$RiceTypes[data$RiceTypes == "thai jasmine rice" & data$Deliciousness == "rice"])
p <- n_thia_jasmine_rice / n_rice
pie <- 70 / 100
z <- (p - pie) / sqrt(pie * (1 - pie) / n)

# Compute Critical value
alpha <- .05 / 2
z.alpha <- qnorm(1 - alpha)

sprintf("computed Z value : %.2f", z)
sprintf("critical Z value : %.2f and %.2f", -z.alpha, z.alpha)

# Compute p-value
p.value <- pnorm(z)
cat("p-value :", p.value, "\n")

if(p.value >= alpha){
  print("Accept H0")
} else {
  print("Reject H0")
}