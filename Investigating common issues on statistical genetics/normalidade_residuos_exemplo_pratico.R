# ═══════════════════════════════════════════════════════════════
# NORMALIDADE DOS RESÍDUOS - Exemplo didático
# Dataset: mtcars (modelo ok) e airquality (violação + correção)
# ═══════════════════════════════════════════════════════════════

# ── ATO 1: Como é quando funciona bem ───────────────────────────
# Modelo: consumo de combustível explicado pelo peso do carro

modelo1 <- lm(mpg ~ wt, data = mtcars)
res1    <- residuals(modelo1)

# Detalhes estatísticos do modelo
summary(modelo1)

# Coeficientes da regressão linear: intercepto e inclinação
modelo1$coefficients

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

hist(res1,
     breaks = 10,
     main   = "Resíduos — mtcars",
     xlab   = "Resíduos",
     col    = "steelblue",
     border = "white")

qqnorm(res1, main = "QQ-plot — mtcars", pch = 16, col = "steelblue")
qqline(res1, col = "red", lwd = 2)

shapiro.test(res1)

# Interpretação:
# W = 0.945, p = 0.104
# Como p > 0.05, não há forte evidência contra a normalidade dos resíduos.
# O histograma é razoavelmente simétrico e os pontos do QQ-plot ficam próximos da linha.


# ── ATO 2: Quando há assimetria — ozônio em Nova York (1973) ─────
# Antes de modelar: entender os dados brutos

dados_ozonio <- na.omit(airquality$Ozone)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

hist(dados_ozonio,
     breaks = 12,
     main   = "Ozônio — distribuição bruta",
     xlab   = "ppb",
     col    = "#e05c5c",
     border = "white")

boxplot(dados_ozonio,
        main = "Ozônio — boxplot",
        col  = "steelblue",
        ylab = "ppb")

# Investigando o maior valor observado
max(dados_ozonio)                        
which(dados_ozonio == max(dados_ozonio))

airquality[airquality$Ozone == max(dados_ozonio) &
             !is.na(airquality$Ozone), ]

# Interpretação:
# O maior valor observado foi 168 ppb.
# Ele ocorreu em 25/08/1973, com temperatura de 81°F, vento de 3.4 mph e radiação solar de 238.
# Dia quente + pouco vento + radiação solar elevada pode favorecer acúmulo de ozônio.
# Portanto, esse valor pode representar um evento real, não necessariamente erro.


# ── ATO 3: Modelo real sem transformação ────────────────────────
# Modelo: concentração de ozônio explicada por variáveis meteorológicas
# Ozone  = concentração de ozônio
# Solar.R = radiação solar
# Wind   = velocidade do vento
# Temp   = temperatura

dados_air <- na.omit(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])

modelo2 <- lm(Ozone ~ Solar.R + Wind + Temp, data = dados_air)
res2    <- residuals(modelo2)

summary(modelo2)

# Diagnóstico da normalidade dos resíduos
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

hist(res2,
     breaks = 12,
     main   = "Resíduos — modelo sem transformação",
     xlab   = "Resíduos",
     col    = "#e05c5c",
     border = "white")

qqnorm(res2,
       main = "QQ-plot — modelo sem transformação",
       pch  = 16,
       col  = "#e05c5c")
qqline(res2, col = "red", lwd = 2)

shapiro.test(res2)

# Interpretação:
# W = 0.91709, p = 3.618e-06
# Como p < 0.05, rejeitamos H0 de normalidade dos resíduos.
# Mesmo usando variáveis meteorológicas reais, os resíduos ainda apresentam
# desvio importante da normalidade, principalmente na cauda superior.


# ── ATO 4: Transformação logarítmica da resposta ─────────────────
# Como Ozone é uma variável positiva e assimétrica à direita,
# usamos log(Ozone) como resposta.
# O log comprime valores altos sem descartar observações reais.

modelo3 <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data = dados_air)
res3    <- residuals(modelo3)

summary(modelo3)

# Diagnóstico dos resíduos após transformação
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

hist(res3,
     breaks = 12,
     main   = "Resíduos — modelo com log(Ozone)",
     xlab   = "Resíduos",
     col    = "#4caf7d",
     border = "white")

qqnorm(res3,
       main = "QQ-plot — modelo com log(Ozone)",
       pch  = 16,
       col  = "#4caf7d")
qqline(res3, col = "darkgreen", lwd = 2)

shapiro.test(res3)

# Interpretação:
# W = 0.97749, p = 0.05726
# Como p > 0.05, não rejeitamos H0 de normalidade ao nível de 5%.
# A transformação log melhorou substancialmente o comportamento dos resíduos:
# o histograma ficou mais simétrico e o QQ-plot ficou mais próximo da linha.


# ── ATO 5: Comparação visual — antes e depois ───────────────────
# Comparação entre:
# modelo2: Ozone ~ Solar.R + Wind + Temp
# modelo3: log(Ozone) ~ Solar.R + Wind + Temp

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

hist(res2,
     breaks = 12,
     main   = "Antes — histograma",
     xlab   = "Resíduos",
     col    = "#e05c5c",
     border = "white")

hist(res3,
     breaks = 12,
     main   = "Depois (log) — histograma",
     xlab   = "Resíduos",
     col    = "#4caf7d",
     border = "white")

qqnorm(res2,
       main = "Antes — QQ-plot",
       pch  = 16,
       col  = "#e05c5c")
qqline(res2, col = "red", lwd = 2)

qqnorm(res3,
       main = "Depois (log) — QQ-plot",
       pch  = 16,
       col  = "#4caf7d")
qqline(res3, col = "darkgreen", lwd = 2)

# Mensagem final:
# No modelo sem transformação, o Shapiro-Wilk indicou forte evidência
# contra a normalidade dos resíduos: p = 3.618e-06.
#
# Após a transformação logarítmica, o p-valor aumentou para 0.05726,
# e o QQ-plot mostrou resíduos muito mais alinhados à distribuição normal.
#
# Portanto, a transformação log melhorou o diagnóstico dos resíduos.
# A decisão, porém, não deve depender apenas do p-valor:
# é preciso considerar também os gráficos, o tipo de variável e o contexto.
