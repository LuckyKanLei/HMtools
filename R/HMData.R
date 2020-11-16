#' @title MetData s4 class
#' @importFrom methods new
#' @export MetData
MetData <- setClass("MetData",
                    slots = c(TAir = "array",
                              TMax = "array",
                              TMin = "array",
                              Precipitation = "array",
                              AirDensity = "array",
                              AirPressure = "array",
                              VaporPressure = "array",
                              VaporPressDeficit = "array",
                              RelativeHumidity = "array",
                              WindSpeed = "array",
                              WindH = "array",
                              ShortWave = "array",
                              LongWave = "array",
                              SunHour = "array"))

#' @title VegData s4 class
#' @importFrom methods new
#' @export VegData
VegData <- setClass("VegData",
                    slots = c(LAI = "array",
                              Albedo = "array",
                              Fetch = "array",
                              VegHeight = "array",
                              TrunkRatio = "array",
                              Displacement = "array",
                              Roughness = "array",
                              WindAttenuation = "array",
                              IsOverstory = "array",
                              MaxIntercept = "array"))

#' @title GeoData s4 class
#' @importFrom methods new
#' @export GeoData
GeoData <- setClass("GeoData",
                    slots = c(Latitude = "array",
                              Longitude = "array",
                              Elevation = "array"))

#' @title TopoData s4 class
#' @importFrom methods new
#' @export TopoData
TopoData <- setClass("TopoData",
                     slots = c(LagOneSlope = "array",
                               SigmaSlope = "array",
                               MaxSnowDistribSlope = "array"))

#' @title SOilData s4 class
#' @importFrom methods new
#' @export SOilData
SOilData <- setClass("SoilData",
                     slots = c(Porosity = "array",
                               FieldCapacity = "array",
                               WiltingPoint = "array",
                               WettingFrontSoilSuctionHead = "array",
                               SaturatedSoilSuctionHead = "array",
                               SaturatedHydraulicConductivity = "array"))

#' @title TimeData s4 class
#' @importFrom methods new
#' @export TimeData
TimeData <- setClass("TimeData",
                     slots = c(NDay = "array",
                               NMon = "array"))


#' @title Atmos s4 class
#' @importFrom methods new
#' @export Atmos
Atmos <- setClass("Atmos",
                  slots = c(PVegVaporFlux = "array",
                            VegVaporFlux = "array"))



#' @title Prec s4 class
#' @importFrom methods new
#' @export Prec
Prec <- setClass("Prec",
                 slots = c(Precipitation = "array",
                           RainFall = "array",
                           SnowFall = "array"))

#' @title Canopy s4 class
#' @importFrom methods new
#' @export Canopy
Canopy <- setClass("Canopy",
                   slots = c(StorageCapacity = "array",
                             SnowStorageCapacity = "array"))


#' @title Energy s4 class
#' @importFrom methods new
#' @export Energy
Energy <- setClass("Energy",
                   slots = c(TCanopy = "array",
                             TSoil = "array",
                             TSnow = "array"))


#' @title Aerodyna s4 class
#' @importFrom methods new
#' @export Aerodyna
Aerodyna <- setClass("Aerodyna",
                     slots = c(AerodynaResist = "array",
                               ArchitecturalResist = "array",
                               StomatalResist = "array",
                               AerodynaResistCanopy = "array",
                               AerodynaResistSnow = "array",
                               ReferHeightCanopy = "array",
                               ReferHeightSnow = "array",
                               RoughCanopy = "array",
                               RoughSnow = "array",
                               DisplacCanopy = "array",
                               DisplacSnow = "array",
                               WindSpeedCanopy = "array",
                               WindSpeedSnow = "array"))


#' @title EvaTrans s4 class
#' @importFrom methods new
#' @export EvaTrans
EvaTrans <- setClass("ET",
               slots = c(RET = "array",
                         ET = "array",
                         Transpiration = "array",
                         Evaporation = "array",
                         EvaporationCanopy = "array",
                         EvaporationLand = "array"))

#' @title Intercept s4 class
#' @importFrom methods new
#' @export Intercept
Intercept <- setClass("Intercept",
                      slots = c(Interception = "array",
                                InterceptRain = "array",
                                InterceptSnow = "array"))


#' @title Ground s4 class
#' @importFrom methods new
#' @export Ground
Ground <- setClass("Ground",
                   slots = c(ZoneMoistureVolume = "array",
                             MoistureVolume = "array",
                             MoistureCapacity = "array",
                             MoistureCapacityMax = "array",
                             HydraulicConductivity = "array",
                             WettingFrontSoilSuction = "array",
                             MoistureContent = "array",
                             EffectivePorosity = "array",
                             ZoneDepth = "array",
                             Runoff = "array",
                             BaseFlow = "array"))


#' @title Infilt s4 class
#' @importFrom methods new
#' @export Infilt
Infilt <- setClass("Infilt",
                   slots = c(Infiltration = "array",
                             InfiltrationRate = "array",
                             InfiltrationRateMax = "array"))

#' @title Snow s4 class
#' @importFrom methods new
#' @export Snow
Snow <- setClass("Snow",
                 slots = c(Boling = "array"))
#' @title HM.Data s4 class
#' @importFrom methods new
#' @export HM.Data
HM.Data <- setClass("HM.Data",
                    slots = c(MetData = "MetData",
                              VegData = "VegData",
                              GeoData = "GeoData",
                              TopoData = "TopoData",
                              SoilData = "SoilData",
                              TimeData = "TimeData",
                              Energy = "Energy",
                              Aerodyna = "Aerodyna",
                              Prec = "Prec",
                              Canopy = "Canopy",
                              ET = "ET",
                              Intercept = "Intercept",
                              Ground = "Ground",
                              Infilt = "Infilt",
                              # Runoff = "Runoff",
                              Snow = "Snow"))

#' @title subsetting for HM.Data
#' @description Extract the data
#' @importFrom  purrr map
#' @param Data the data, from it get the required data
#' @param i index
#' @return Locat whith the required data
#' @export
'[.HM.Data' <- function(Data, i){
  Out <- map(Data, getListI, i)
  class(Out) <- class(Data)
  return(Out)
}
#' @title getListI
#' @description Extract the data required by Locat in Data for list
#' @import purrr
#' @param List the data, from it get the required data
#' @param i index
#' @return Locat whith the required data
#' @export
getListI <- function(List, i){
  Out <- map(List, getI, i)
  # class(Out) <- class(List)
  return(Out)
}
#' @title subsetting geben for HM.Data
#' @description Extract the data required by Locat in Data for list
#' @param Data the data, from it get the required data
#' @param i index
#' @param value value
#' @return new data
#' @export
'[<-.HM.Data' <- function(Data, i, value){
  class(Data) <- "list"
  class(value) <- "list"

  DaNa <- names(Data)
  PuNa <- names(value)
  INa <- intersect(DaNa,PuNa)
  # indexPI <- as.integer(map(INa, function(a, b)which(b %in% a), PuNa))
  # PutData <- PutData[indexPI]

  for (j in INa) {
    Data[[j]] <- putList(Data[[j]], i, value[[j]])
    # eval(parse(text = paste0("Data$", INa[j], " <- ", "putList(", "value$", INa[j], ",", "Data$", INa[j], ",", i,")")))
  }
  class(Data) <- "HM.Data"
  return(Data)
}
#' @title putList
#' @description put the data to a HM.Data
#' @param Data the data, from it get the required data
#' @param i index
#' @param PutData PutData
#' @return list
#' @export
putList <- function(Data, i, PutData){
  DaNa <- names(Data)
  PuNa <- names(PutData)
  INa <- intersect(DaNa,PuNa)
  for(j in INa){
    dimN <- length(dim(Data[[j]]))
    if(dimN == 2) Data[[j]][i,] <- PutData[[j]]
    else if(dimN == 3) Data[[j]][i,,] <- PutData[[j]]
    else Data[[j]][i] <- PutData[[j]]

    # dimN <- length(dim(parse(text = paste0("Data$",INa[j]))))
    # if(dimN == 2) eval(parse(text = paste0("Data$", INa[j], "[i,]", " <- ", "PutData$", INa[j])))
    # else if(dimN == 3) eval(parse(text = paste0("Data$", INa[j], "[i,,]", " <- ", "PutData$", INa[j])))
    # else eval(parse(text = paste0("Data$", INa[j], "[i]", " <- ", "PutData$", INa[j])))

  }
  return(Data)
}

#' @title mergeData
#' @description merge the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeData.HM.Data <- function(Ori, New){
  clNa <- class(Ori)
  class(Ori) <- "list"
  class(New) <- "list"
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  DNNa <- setdiff(NeNa, OrNa)
  for (i in DNNa) {
    Ori[[i]] <- New[[i]]
  }
  for(i in INa){
    Ori[[i]] <- mergeList(Ori[[i]], New[[i]])
  }
  class(Ori) <- clNa
  return(Ori)
}
#' @title mergeData
#' @description merge the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeList <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  DNNa <- setdiff(NeNa, OrNa)
  for (i in NeNa) {
    Ori[[i]] <- New[[i]]
  }
  return(Ori)
}

