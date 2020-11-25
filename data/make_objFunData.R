objFun <- readRDS("funData.rda")

order(names(objFun))

# objFun[["Ackley-2D"]] <- smoof::makeAckleyFunction(2)
# objFun[["Himmelblau"]] <- smoof::makeHimmelblauFunction()
# objFun["Styblinski-Tang"] <- smoof::makeStyblinkskiTangFunction()

# objFun[["Booth"]] <- smoof::makeBoothFunction()
# objFun[["Brent"]] <- smoof::makeBrentFunction()
# objFun[["Brown"]] <- smoof::makeBrownFunction(2)
# objFun[["Complex"]] <- smoof::makeComplexFunction()
# objFun[["Cube"]] <- smoof::makeCubeFunction()
# objFun[["Dent"]] <- smoof::makeDentFunction()
objFun[["Schwefel"]] <- smoof::makeSchwefelFunction(2)
objFun[["Periodic"]] <- smoof::makePeriodicFunction()
objFun[["HyperEllipsoid"]] <- smoof::makeHyperEllipsoidFunction(2)
objFun[["Hosaki"]] <- smoof::makeHosakiFunction()
objFun[["Jennrich-Sampson"]] <- smoof::makeJennrichSampsonFunction() #?
objFun[["Sphere"]] <- smoof::makeSphereFunction(2) #?
objFun[["Giunta"]] <- smoof::makeGiuntaFunction()
objFun[["Generalized-Drop-Wave"]] <- smoof::makeGeneralizedDropWaveFunction()
objFun[["Exponential"]] <- smoof::makeExponentialFunction(2)

# order list by names
objFun = objFun[order(names(objFun))]

#save(list= names(objFun), file = "objFunData.rda", envir = as.environment(objFun))
saveRDS(objFun, "objFunData.RDS")
