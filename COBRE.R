library(tseries)
library(openxlsx)
library(forecast)
library(trend)
library(TSA)
library(randtests)
library(funtimes)
library(modifiedmk)
library(pspearman)
library(tidyverse)
library(car)
library(ggplot2)
library(psych)
library(rcompanion)
library(readxl)
library(astsa)
library(fGarch)
library(FinTS)
library(ggfortify)
library(stats)
library(LSTS)
library(ICglm)
library(stargazer)
library(lmtest)
library(strucchange)
library(TSstudio)
library(urca)
library(lmtest)
cobre<- read_excel("D:/ALE2024/COBRE/COBRE.xlsx")
View(cobre)
# verificando si existen datos vacios
any(is.na(cobre))
str(cobre)
# grafica serie completa 1999-2024
  ts_produccion<-ts(cobre$total_prod,start=c(1999,1),frequency = 12)
    plot.ts(ts_produccion,col="blue", ylab = "Producción total", xlab = "Años",lwd = 2)
    abline(v=2024)
    # DATA ENTRENAMIENTO 1999-2023
    cobre_entre <- cobre%>% 
      filter(AÑO < 2024)
    dim(cobre_entre)
    # ordenar los meses
    cobre_entre$MES <- factor(cobre_entre$MES,
                              levels = c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN",
                                         "JUL", "AGO", "SET", "OCT", "NOV", "DIC"),
                              ordered = TRUE)
    
    # DATA DE PRUEBA ENERO-AGOSTO 2024
    cobre_prueba<-cobre%>%
      filter(AÑO>=2024)
    dim(cobre_prueba)
    # ESTADISTICOS DESCRIPTIVOS DE LA DATA DE ENTRENAMIENTO
    summary(cobre_entre$total_prod)
    tasa_mensual <- (cobre_entre$total_prod - lag(cobre_entre$total_prod, 1)) / lag(cobre_entre$total_prod, 1) * 100
    mean(tasa_mensual,na.rm=TRUE)
    describe(cobre_entre$total_prod)
    boxplot(cobre_entre$total_prod, col = "white",ylab = "Producción total")
    boxplot(total_prod~ MES, data = cobre_entre, 
            xlab = "Mes", ylab = "Producción total", col = "white")
    boxplot(total_prod~ AÑO, data = cobre_entre, 
            xlab = "Año", ylab = "Producción total", col = "white")
    
    # GRAFICA SERIE ENTRENAMIENTO y PRUEBA
    ts_produccion_entre <- ts(cobre_entre$total_prod, start = c(1999, 1), frequency = 12)
    ts_produccion_prueba <- ts(cobre_prueba$total_prod, start = c(2024, 1), frequency = 12)
    plot.ts(ts_produccion_entre, col = "blue", 
            ylab = "Producción total", 
            xlab = "Años",
            lwd = 2)
    # DESCOMPOSICION DE LA SERIE
    ts_produccion_components <- decompose(ts_produccion_entre)
    plot(ts_produccion_components)
    # CARACTERISTICAS DE LA SERIE ENTRENAMIENTO
    # 1) TENDENCIA
    # h0: no esta presente el componente tendencia
    # h1: esta presente el componente de tendencia
    # MANN KENDALL
    mk.test(ts_produccion_entre, continuity = TRUE)
    # SI HAY TENDENCIA
    
    # 2) ESTACIONALIDAD
    #  h0: no esta presente el componente estacionalidad
    #  h1: esta presente el componente de estacionalidad
    # KRUSKAL-WALLIS
    kruskal.test(total_prod ~MES,cobre_entre)#mensual
    kruskal.test(total_prod ~AÑO,cobre_entre) #anual
    # NO ESTACIONALIDAD MENSUAL Y SI ESTACIONALIDAD AÑUAL
    
    # 3) ESTACIONARIEDAD
    # H0: no es estacionaria
    # H1: es estacionaria
    adf.test(ts_produccion_entre)
    # NO ES ESTACIONARIEDAD
    
    # 4) HETEROCEDASTICIDAD
    # H0: existe homocedasticidad
    # H1: exite heterocedasticidad
    leveneTest(cobre_entre$total_prod ~ as.factor(cobre_entre$MES))
    leveneTest(cobre_entre$total_prod ~ as.factor(cobre_entre$AÑO))
    # MENSUAL HOMOCEDASTICIDAD Y ANUAL HETEROCEDASTICIDAD
    
    ####### METODOLOGIA DE BOX JENKINS ######
    # CORRELOGRAMAS DE SERIE ENTRENAMIENTO
    layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE))
    plot.ts(ts_produccion_entre, col = "blue", 
            ylab = "Producción total", 
            xlab = "Años",
            main= "(a)",
            lwd = 1)
    acf(ts_produccion_entre,lwd=2,main="(b)",lag.max=50)# decaimento lento no estacionario
    pacf(ts_produccion_entre,lwd=2,main="(c)",lag.max=50)
    # RESOLVEMOS HETEROCEDASTICIDAD
    # transformacion BOX COX
    lx = log(ts_produccion_entre)#lambda=0
    par(mfrow=c(1,1))
    plot.ts(lx, col = "blue", 
            ylab = "Producción total", 
            xlab = "Años",
            main= "",
            lwd = 1)
    leveneTest(lx~as.factor(cycle(lx)))#homocedastico
    mk.test(lx, continuity = TRUE)#tendencia
    # SOLUCIONAMOS ESTACIONARIEDAD
    dlx=diff(lx)#d=1
    adf.test(dlx)#estacionaria
    mk.test(dlx, continuity = TRUE)
    layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE))
    plot.ts(dlx, col = "blue", 
            ylab = "Producción total", 
            xlab = "Años",
            main= "(a)",
            lwd = 1)
    acf(dlx,lwd=2,main="(b)",lag.max=50)
    pacf(dlx,lag.max = 50,lwd = 2,main="(c)")
    #SOLUCIONAMOS ESTACIONALIDAD
    dlx_s=diff(dlx,lag=12)
    plot.ts(dlx_s, col = "blue", 
            ylab = "Producción total", 
            xlab = "Años",
            main= "(a)",
            lwd = 1)
    acf(dlx_s,lwd=2,main="(b)",lag.max=50)
    pacf(dlx_s,lag.max = 50,lwd = 2,main="(c)")
    
    # POSIBLES MODELOS
    model1 = Arima(lx, order = c(0,1,0), seasonal = list(order = c(1,1,1), period = 12), include.mean = FALSE)
    model2 = Arima(lx, order = c(0,1,0), seasonal = list(order = c(2,1,0), period = 12), include.mean = FALSE)
    model3 = Arima(lx, order = c(1,1,1), seasonal = list(order = c(0,1,2), period = 12), include.mean = FALSE)
    model4 = Arima(lx, order = c(1,1,1), seasonal = list(order = c(1,1,1), period = 12), include.mean = FALSE)
    model5 = Arima(lx, order = c(1,1,2), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE)
    model6 = Arima(lx, order = c(1,1,2), seasonal = list(order = c(0,1,2), period = 12), include.mean = FALSE)
    model7 = Arima(lx, order = c(1,1,2), seasonal = list(order = c(1,1,1), period = 12), include.mean = FALSE)
    model8 = Arima(lx, order = c(2,1,0), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE)
    model9 = Arima(lx, order = c(2,1,0), seasonal = list(order = c(1,1,1), period = 12), include.mean = FALSE)
    model10 = Arima(lx, order = c(2,1,0), seasonal = list(order = c(2,1,0), period = 12), include.mean = FALSE)
    model11 = Arima(lx, order = c(2,1,1), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE)
    model12 = Arima(lx, order = c(2,1,2), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE)
    # PARAMETROS ESTIMADOS AIC BIC
    summary(model1)
    summary(model2)
    summary(model3)
    summary(model4)
    summary(model5)
    summary(model6)
    summary(model7)
    summary(model8)
    summary(model9)
    summary(model10)
    summary(model11)
    summary(model12)
    # SIGNIFICANCIA DE LOS PARAMETROS ESTIMADOS
    printstatarima <- function (x, digits = 4,se=T,...){
      if (length(x$coef) > 0) {
        cat("\nCoefficients:\n")
        coef <- round(x$coef, digits = digits)
        if (se && nrow(x$var.coef)) {
          ses <- rep(0, length(coef))
          ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
          coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
          coef <- rbind(coef, s.e. = ses)
          statt <- coef[1,]/ses
          pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = FALSE)
          coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
          coef <- t(coef)
        }
        print.default(coef, print.gap = 2)
      }
    }
    printstatarima(model1)
    printstatarima(model2)
    printstatarima(model3)
    printstatarima(model4)
    printstatarima(model5)
    printstatarima(model6)
    printstatarima(model7)
    printstatarima(model8)
    printstatarima(model9)
    printstatarima(model10)
    printstatarima(model11)
    printstatarima(model12)
    # VERIFICACION DE LOS MODELOS CON SIGNIFICANCIA
    # grafica para cada uno de los 4 modelos cambiando el modelo
    layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE))
    plot(resid(model2), 
         ylab = "", 
         xlab = "Años",
         main= "Residuales estandarizados",
         lwd = 1)#residuales
    acf(resid(model2),main="ACF residuales")#ACF RESIDUALES 
    pacf(resid(model2),main="PACF residuales")#PACF RESIDUALES
    
    #=========GRAFICA DE LJUNG BOX Y Q-Q PLOT==========
    # En residuales cambiar para cada modelo
    res <- resid(model10)
    # Calcular p-valores del test de Ljung-Box para diferentes rezagos
    par(mfrow=c(2,1))
    gof_lag <- 10  # Número de rezagos
    p_values <- sapply(1:gof_lag, function(lag) {
      Box.test(res, lag = lag, type = "Ljung-Box")$p.value
    })
    plot(1:gof_lag, p_values, type = "p", pch = 1,lwd = 2, col = "black",
         xlab = "Lag", ylab = "p-value", main = "P-values for Ljung-Box statistic",
         ylim = c(0, 1))  # Límite de p-valores entre 0 y 1
    abline(h = 0.05, col = "red", lty = 2)  # Línea de significancia
    qqnorm(res, main = "Normal Q-Q Plot of Residuals", pch = 1, col = "black")
    qqline(res, col = "red", lwd = 1)
    
    # ruido blanco
    Box.test(resid(model2),lag=12, type = "Ljung-Box")#no
    Box.test(resid(model5), lag=12, type = "Ljung-Box")
    Box.test(residuals(model8), lag = 12, type = "Ljung-Box")
    Box.test(residuals(model10), lag = 12, type = "Ljung-Box")
    
    #normalidad 
    jarque.bera.test(model2$resid)#NO
    jarque.bera.test(model5$resid)
    jarque.bera.test(model8$residuals)
    jarque.bera.test(model10$residuals)
    
    #=========PREDICCION===========
    # MODELO5
    pron5 = predict(model5, n.ahead=8) 
    expron5<-round(exp(pron5$pred),0)
    expron5
    #MODELO8
    pron8<-predict(model8,n.ahead = 8)
    expron8<-round(exp(pron8$pred),0)
    expron8
    
    #MODELO10
    pron10<-predict(model10,n.ahead = 8)
    expron10<-round(exp(pron10$pred),0)
    expron10
    
    #GRAFICA DE PRONOSTICO
    par(mfrow=c(1,1))
    ultimos_meses_original <- tail(ts_produccion, 50)   # Últimos 8 valores
    plot(ultimos_meses_original, type="l", col="blue", 
         xlab="Año", ylab="Producción Total", main="")
    lines(expron10, col="black", lwd=2)#PROBAR PARA LOS MODELOS VALIDADOS
    legend("topleft", legend=c("Serie Original", "Pronóstico"),
           col=c("blue", "black", "red"), lty=c(1, 1, 2), lwd=c(1, 2, 1))
    
    #======METRICAS=====
    # En pronosticados probar con cada uno de los modelos
    prueba <- tail(ts_produccion, 8)  
    pronosticados <- expron10     
    
    rmse <- sqrt(mean((prueba - pronosticados)^2))
    mae <- mean(abs(prueba - pronosticados))
    mape <- mean(abs((prueba - pronosticados) / prueba)) * 100
    sse <- sum((prueba - pronosticados)^2)
    
    cat("RMSE: ", rmse, "\n")
    cat("MAE: ", mae, "\n")
    cat("MAPE: ", mape, "%\n")
    cat("SSE: ", sse, "\n")
    