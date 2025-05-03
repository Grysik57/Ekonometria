library(readxl)
#skoroszyt "wage" zawiera dane o zarobkach mieszkańców Belgii 
#podane w eur/h oraz informacje o płci, wykształceniu i doświadczeniu.
wage <- read_excel("/Users/michal/UEK/R/dane.xlsx", sheet = "wage")
educ_sqr <- wage$`EXPER^2`
#dopasowanie modelu regresji liniowej do danych
reg <- lm(WAGE ~ MALE + EDUC + EXPER + educ_sqr, data = wage)
summary(reg)
#Można zauważyć że na pensję istotnie wpływa płec oraz wykształcenie.

sum(reg$residuals^2)
mean(reg$residuals^2)


r <- matrix(c(0, 0, 0, 1, 2*mean(wage$EXPER)), nrow = 1, ncol = 5)
beta_hat <- matrix(reg$coefficients)

gamma_hat <- r%*%beta_hat
#zbadanie zależności wariancji
model <- aov(WAGE ~ MALE + EDUC + EXPER + educ_sqr, data = wage)
summary(model)

#Ocena macierzy kowariancji
v <- vcov(reg)
D_gamma_hat <- ((r%*%v)%*%t(r))^(1/2)

#Test T-Studenta
badana <- 0.05

T <- (gamma_hat-badana/D_gamma_hat)
T
p_value <- 1 - pt(T, reg$df.residual)
p_value





