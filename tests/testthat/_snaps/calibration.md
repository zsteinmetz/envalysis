# Snapshot output consistent

    
    Call:
    calibration(formula = Area ~ Conc, data = din32645)
    
    Coefficients:
    (Intercept)         Conc  
           2481         9662  
    
    Adjusted R-squared:  0.983
    Sum relative error:  0.7978
    
    Blanks:
     [1] 2003 1901 2212 1976 2279 1853 2165 2108 2368 1943
    
         Conc   lwr   upr
    LOD 0.053 0.036 0.096
    LOQ 0.212 0.146 0.387

---

    
    Call:
    calibration(formula = Meas ~ Conc, data = neitzel2003)
    
    Coefficients:
    (Intercept)         Conc  
        0.02753      2.48461  
    
    Adjusted R-squared:  0.9975
    Sum relative error:  0.416
    
    Blanks:
     [1] 0.027 0.033 0.041 0.028 0.018 0.022 0.029 0.038 0.024 0.023
    
         Conc   lwr   upr
    LOD 0.009 0.006 0.016
    LOQ 0.086 0.059 0.157

