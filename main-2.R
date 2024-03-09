# เชื่อว่าเป็นร้อยละ 70% ของสัดส่วนนิสิตที่ชื่นชอบการรับประทานข้าวหอมมะลิ จากการสุ่มตัวอย่าง 64 คน มีนิสิตที่ชอบรับประทานข้าวอยู่ 30 คน
# มีนิสิตที่ชอบรับประทานข้าวหอมมะลิอยู่ 24 คน จึงทดสอบว่าสัดส่วนนิสิตที่ชอบรับประทานข้าวหอมมะลิเป็น 70% จริงหรือไม่ที่ระดับนัยสำคัญ 0.05
# กำหนดให้ p แทนสัดส่วนของนิสิตที่ชื่นชอบการรับประทานข้าวหอมมะลิในกลุ่มตัวอย่าง
# π แทนสัดส่วนของนิสิตที่ชื่นชอบการรับประทานข้าวหอมมะลิในกลุ่มประชากร
# ตั้งสมมุติฐาน:
# H0 : π = 0.7
# H1 : π != 0.7
# กำหนดระดับนัยสำคัญ α = 0.05

data <- read.csv(file="C:\\Users\\ACER USER5949486\\Desktop\\CodeR\\data.csv")
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
z.alpha <- qnorm(1 - alpha) # 1.96

sprintf("computed Z value : %.2f", z)
sprintf("critical Z value : %.2f", z.alpha)

# Compute p-value
p.value <- pnorm(z)
cat("p-value :", p.value, "\n")

if(p.value >= alpha){
  print("Accept H0")
} else {
  print("Reject H0")
}