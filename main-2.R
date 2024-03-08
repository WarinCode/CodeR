data <- read.csv(file="C:\\Users\\ACER USER5949486\\Desktop\\CodeR\\data.csv")

# View(data)

# เชื่อว่าเป็นร้อยละ 60% ของสัดส่วนนิสิตที่ชื่นชอบการรับประทานข้าวมีมากกว่านิสิตที่ชื่นชอบการรับประทานก๋วยเตี๋ยว จากการสุ่มตัวอย่าง 64 คน
# มีนิสิตที่ชอบรับประทานข้าวมีอยู่ 30 คน ทดสอบว่าสัดส่วนนิสิตที่ชอบรับประทานข้าวมีมากกว่านิสิตที่ชอบรับประทานก๋วยเตี๋ยวในระดับนัยสำคัญ 0.05
# กำหนดให้ p แทนสัดส่วนของนิสิตที่ชื่นชอบการรับประทานข้าวในกลุ่มตัวอย่าง
# π แทนสัดส่วนของนิสิตที่ชื่นชอบการรับประทานข้าวในกลุ่มประชากร
# ตั้งสมมุติฐาน:
# H0 : π = 0.6
# H1 : π != 0.6
# กำหนดระดับนัยสำคัญ α = 0.05

# Compute z-value
n <- nrow(data) # 64
n_rice <- length(data$Deliciousness[data$Deliciousness == "rice"]) # 34
p <- n_rice / n
pie <- 60 / 100;
z <- (p - pie) / sqrt(pie * (1 - pie) / n)

# Compute Critical value
alpha <- .05 / 2
z.alpha <- qnorm(1 - alpha) # 1.96

sprintf("computed Z value : %.2f", z)
sprintf("critical Z value : %.2f", z.alpha)

# Compute p-value
p.value <- pnorm(z)
cat("p-value :" , p.value, "\n")

if(p.value >= alpha){
  print("Accept H0")
} else {
  print("Reject H0")
}