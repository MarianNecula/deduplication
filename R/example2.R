#' Example of using deduplication package - the long way
#' 
#' #This is just an example on how to compute duplicity probabilities using simulated data. All the files used in this
#' #example are supposed to be produced using the simulation software. The "simulation.xml" file is an exception and it
#' #is an input file for the simulation software. The files used in this example are provided with the deduplication
#' #package.
#'
#' 
#' # set the folder where the necessary input files are stored
#' 
#' path_root      <- 'extdata'
#' 
#' # 0. Read simulation params
#' 
#' simParams <-readSimulationParams(system.file('extdata', 'simulation.xml', package = 'deduplication'))
#'
#'  # 1. Read grid parameters
#'  
#' gridParams <-readGridParams(system.file('extdata', 'grid.csv', package = 'deduplication'))
#'
#'  # 2.Read network events
#'  
#' events <- readEvents(system.file('extdata', 'AntennaInfo_MNO_MNO1.csv', package = 'deduplication'))
#'
#'  # 3. Get a list of detected devices
#'  
#' devices <- getDeviceIDs(events)
#'
#' #4. Get connections for each device
#' 
#' connections <- getConnections(events)
#'
#' #5. Emission probabilities are computed from the signal strength/quality file
#' 
#' emissionProbs <- getEmissionProbs(gridParams$nrow, gridParams$ncol, system.file('extdata', 'SignalMeasure_MNO1.csv', package = 'deduplication'), simParams$conn_threshold)
#'
#' #6. Build joint emission probabilities
#' 
#' jointEmissionProbs <- getEmissionProbsJointModel(emissionProbs)
#' 
#' #7. Build the generic model
#' 
#' model <- getGenericModel(gridParams$nrow, gridParams$ncol, emissionProbs)
#'
#' #8. Fit models
#' 
#' ll <- fitModels(length(devices), model,connections)
#' 
#' #9. Build the joint model
#' 
#' modelJ <- getJointModel(gridParams$nrow, gridParams$ncol, jointEmissionProbs)
#'
#' #10. Read antenna cells and build a matrix of neighboring antennas
#' 
#' coverarea <- readCells(system.file('extdata', 'AntennaCells_MNO1.csv', package = 'deduplication'))
#' 
#' antennaNeigh <- antennaNeighbours(coverarea)
#' 
#' #11. Apriori probability of duplicity
#' 
#' P1 <- aprioriDuplicityProb(simParams$prob_sec_mobile_phone, length(devices))
#' 
#' #12. Build a matrix of pairs of devices to compute duplicity probability
#' 
#' pairs4dup<-computePairs(connections, length(devices), oneToOne = FALSE, P1=P1, limit = 0.05, antennaNeighbors = antennaNeigh)
#' 
#' #13. Compute duplicity probabilities using the "pairs" method (faster)
#' 
#' out1 <- computeDuplicityBayesian("pairs", devices, pairs4dup, modelJ, ll, P1)
#' 
#' #14. Apriori probability of 2-to-1 
#' 
#' Pii <- aprioriOneDeviceProb(simParams$prob_sec_mobile_phone, length(devices))
#'
#' #15. Build a matrix of pairs of devices to compute duplicity probability
#' 
#' pairs4dup<-computePairs(connections, length(devices), oneToOne = TRUE)
#' 
#' #16. Compute duplicity probabilities using "1to1" method
#' 
#' out2 <- computeDuplicityBayesian("1to1", devices, pairs4dup, modelJ, ll, P1 = NULL, Pii=Pii)
#' 
example2 <- function() {}