# การทดสอบสมมติฐานประชากรเดียว
# เราตั้งสมมุติฐานว่าถ้านิสิตที่รับประทานอาหาร ข้าว และ ก๋วยเตี๋ยว นั้นมีการใช้จ่ายรายวันอย่างน้อย 100 บาทใน 1วัน จึงทดสอบว่าสมมุติฐานเป็นจริงหรือไม่ที่ระดับนัยสำคัญ 0.05
# สมมุติฐานหลัก H0: μ >= 100 (นิสิตที่รับประทานอาหาร ข้าว และ ก๋วยเตี่ยว มีการใช้จ่ายจำนวนเงินอย่างน้อย 100 บาทใน 1 วัน)
# กำหนดให้ H1: μ < 100 (นิสิตที่รับประทานอาหาร ข้าว และ ก๋วยเตี่ยว มีการใช้จ่ายจำนวนเงินน้อยกว่า 100 บาทใน 1 วัน)
# xbar แทนจำนวนเงินโดยเฉลี่ยที่ใช้จ่ายรับประทานอาหารใน 1วัน
# mu แทนจำนวนเงินโดยเฉลี่ยที่ตั้งสมมุติฐานขึ้น
# กําหนดระดับนัยสําคัญ α = 0.05

data <- read.csv(file="C:\\Users\\ACER USER5949486\\Desktop\\CodeR\\data.csv")
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
