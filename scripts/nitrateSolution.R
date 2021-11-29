library(isoToolsR)
# Objective: calculate masses of labeled and unlabeled nitrate needed in a 1 L 
# slug solution to raise the total concentration in each flume by 220 ppb as N 
# and to enrich the nitrate pool to 13,000 permil. Assume 10 mL will be added
# into each flume, and that volume of water in each flume is 30 L. 

V_flume <- 30 # volume of water in flume, Liters
V_slug <- 0.01 # volume of slug, Liters
V_slugSol <- 1 # volume of slug solution to prepare, Liters

RstN <- 0.003678 # isotope ratio of nitrogen standard
AFadd <- 0.99 # atom fraction of N15-nitrate tracer
deltaTarget <- 13000 # target enrichment level in SAM, permil

C_nitrate <- 220/14.0067 # concentration of nitrate + nitrite in SAM after adding nitrate slug, umol/L N
C_N14nitrate <- C_nitrate * (1- calcPercentTracer(deltaTarget, RstN, AFadd))# concentration of N14-nitrate in SAM after adding nitrate slug, umol/L N
C_N15nitrate <- C_nitrate * calcPercentTracer(deltaTarget, RstN, AFadd) # concentration of N15-nitrate in SAM after adding nitrate slug, umol/L N

MW_K15NO3 <- 102.1 # Molecular weight of N15-KNO3
MW_K14NO3 <- 101.1 # Molecular weight of N14-KNO3

C_N14nitrate_slug <- C_N14nitrate*(V_flume + V_slug)/V_slug #Concentration of N14-NO3 in slug solution, umol/L
C_N15nitrate_slug <- C_N15nitrate*(V_flume + V_slug)/V_slug #Concentration of N15-NO3 in slug solution, umol/L

m_K15NO3 <- C_N15nitrate_slug * MW_K15NO3/1000 # mass of K15-NO3 to add, mg
m_K14NO3 <- C_N14nitrate_slug * MW_K14NO3/1000 # mass of K14-NO3 to add, mg

V_max <- 34 # maximum volume of water in any flume, Liters
C_nitrate_min <- (C_N14nitrate_slug + C_N15nitrate_slug) * V_slug/(V_max + V_slug) # Minimum concentration of nitrate in any flume
