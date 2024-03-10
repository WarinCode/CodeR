### การวิเคราะห์การถดถอยอย่างง่าย (สำหรับทำตารางข้อมูล)

data <- read.csv(file="data.csv")
View(data)

# จำนวนคนทั้งหมดที่รับประทานก๋วยเตี๋ยว
n <- nrow(data)
# จำนวนครั้งที่รับประทานก๋วยเตี๋ยวใน 1 สัปดาห์
x <- data$NumberOfTimesEatingNoodlePerAWeek
# จำนวนเงินที่ใช้จ่ายในการรับประทานก๋วยเตี๋ยว
y <- data$Pay
xy <- x * y
x2 <- x ^ 2
y2 <- y ^ 2

# สร้าง dataframe สำหรับทำตารางข้อมูล
df <- data.frame(
  No = 1:64,
  X = x,
  Y = y,
  XY = xy,
  X2 = x2,
  Y2 = y2
)

# หาผลรวมของข้อมูล
sigma_x <- sum(x)
sigma_y <- sum(y)
sigma_xy <- sum(xy)
sigma_x2 <- sum(x2)
sigma_y2 <- sum(y2)

# หาค่าเฉลี่ย
xbar <- mean(x)
ybar <- mean(y)
xybar <- mean(xy)
x2bar <- mean(x2)
y2bar <- mean(y2)

# สร้างแถวใหม่เพื่อใส่ผลรวมของข้อมูลกับค่าเฉลี่ย
df <- rbind(df, c("sum", sigma_x, sigma_y, sigma_xy, sigma_x2, sigma_y2))
df <- rbind(df, c("mean", xbar, ybar, xybar, x2bar, y2bar))
# แสดงผลตาราง
df

# หาค่า a และ b จากสูตร
b <- ((n * sigma_xy) - (sigma_x * sigma_y)) / ((n * sigma_x2) - (sigma_x^2))
a <- ybar - (b * xbar)
# ได้ค่า yhat เพื่อมาทำเป็นสมการถดถอยอย่างง่าย
xi <- 1
y_hat <- a + (b * xi)

# แสดงค่าออกมา
cat("b = ", b, "\n")
cat("a = ", a, "\n")
sprintf("yhat = %g + %gxi", a, b)
sprintf("yhat = %.2f", y_hat)