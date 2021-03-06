% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeDuplicity.R
\name{computeDuplicity}
\alias{computeDuplicity}
\title{Computes the duplicity probability for each device.}
\usage{
computeDuplicity(
  method,
  gridFileName,
  eventsFileName,
  signalFileName,
  antennaCellsFileName = NULL,
  simulatedData = TRUE,
  simulationFileName,
  netParams = NULL,
  path = NULL,
  gamma = 0.5,
  aprioriProbModel = NULL,
  aprioriProbJointModel = NULL,
  lambda = NULL,
  handoverType = "strength",
  emissionModel = NULL,
  antennaFileName = NULL
)
}
\arguments{
\item{method}{The method used to compute the duplicity probability. It could have one of the following values:
"pairs", "1to1", "trajectory".}

\item{gridFileName}{The name of the file with the grid parameters. This file could be the one generated by the
simulation software or can be created with any text editor. The grid file generated by the simulation software has
the following columns: \code{Origin X, Origin Y, X Tile Dim, Y Tile Dim, No Tiles X, No Tiles Y}. We are interested
only in the number of rows and columns and the tile size on OX and OY axes. Therefore, the file provided as input to
this function should have at least the following 4 columns: 'No Tiles X' ,'No Tiles Y, 'X Tile Dim' and 'Y Tile
Dim'.}

\item{eventsFileName}{The name of the file with the network events to be used. Depending on the parameter
\code{simulatedData} it could be a .csv file coming from the simulation software or from a real MNO. In case the
file comes from the simulation software it contains following columns: \code{time, antennaID, eventCode, deviceID,
x, y, tile} Only the first 4 columns are used, the rest are ignored.}

\item{signalFileName}{The name of the .csv file that contains the signal strength/quality for each tile in the grid.
Depending on the parameter \code{simulatedData} it could be a .csv file coming from the simulation software or from
a real MNO. In case the file comes from the simulation software the data are organized as a matrix with the number
of rows equals to the number of antennas and the the following columns: \code{Antenna ID, Tile 0, Tile 1, ... Tile
(N-1)}. On the first column there are the antenna IDs and on the rest of the columns the coresponding signal
strength/quality for each tile in the grid.}

\item{simulatedData}{If TRUE the input data are provided by the simulation software, otherwise real data is used.}

\item{simulationFileName}{The name of the file used to define a simulation scenario. It is the file that was provided
as input for the simulation software. This file is required only if simulatedData is TRUE.}

\item{netParams}{This parameter is required if simulatedData is FALSE, i.e. the deduplication package uses real mobile
network data. In this case, netParam is a list with two elements: conn_threshold and prob_sec_mobile_phone.
conn_threshold is the minimum value of the signal strength/quality that can be used to connect a mobile device to an
antenna. If the signal in a region is below this value the region is considered out of the coverage area.
prob_sec_mobile_phone is the probability of a person to have two mobile devices.}

\item{path}{The path where the files with the posterior location probabilities for each device are to be found. This
parameter is needed only if the "trajectory" method is used.}

\item{gamma}{This value is used only for the "trajectory" method and is the factor used in the compariosn between the
mode of Delta X and Delta Y and the dispersion radius. The default value is 0.5.}

\item{aprioriProbModel}{This parameter is used to initialize the apriori probabilities for the HMM model for each
device. The default value is NULL which means that by default the models are initialized with the steady state.}

\item{lambda}{This parameter is used in combination with the "1-to-1" method. If it is NULL (which is the default
value) the computations follow the description given in
\href{https://webgate.ec.europa.eu/fpfis/mwikis/essnetbigdata/images/f/fb/WPI_Deliverable_I3_A_proposed_production_framework_with_mobile_network_data_2020_05_31_draft.pdf}{WPI
 Deliverable 3}, otherwise the coputations proceed as is the \emph{An end-to-end statistical process with mobile
network data for Official Statistics} paper.}

\item{handoverType}{The handover mechanism used by the mobile network. It could have two values: "strength" or
"quality". The default value is "strength". This parameter is used to compute the emission probabilities for the HMM
models using the values from the signal file. If this file contains the signal strength then the handoverType should
be set to "strength" and if the file contains the values of the signal quality then the parameter should be set to
"quality".}

\item{emissionModel}{This parameter dictates how the emission probabilities for the HMM models are computed. It can
have two values: "RSS" and "SDM". Normally, the emission probabilities are computed using the signal values from the
signal file assuming that if the handoverType is set to "strength" then the signal file contains the strength values
and if the handoverType is set to "quality" the signal file contains the quality of the signal. For demonstrative
purposes the package supports unsual combination. If handoverType is set to "strength", the signal file contains the
signal strength but the emissionModel is set to "SDM" the signal strength values are transformed to signal quality
and then the computation of the emission probablities uses these transformed values. If handoverType is set to
"quality", the signal file contains the signal quality but the emissionModel is set to "RSS" the signal quality
values are transformed to signal strength and then the computation of the emission probablities uses these
transformed values.}

\item{antennaFileName}{The name of the xml file containing the technical parameters of the antennas needed to
transform the signal quality into signal strength and vice versa. This is an input file of the simulation software.
An example file is included in this package. The default value is NULL because this file is only needed to make
unusual combinations between handoverType and the way the emission probabilities are computed.}

\item{cellsFileName}{It is the name of the file where the coverage areas of antennas are to be found. It is a should
be a .csv file with two values on each row: the antenna ID and a WKT string representing a polygon (i.e. it should
start with the word POLYGON) which is the coverage area of the corresponding antenna. This area is also called the
antenna cell.}

\item{aprioriJointModel}{This parameter is used to initialize the apriori probabilities for the joint HMM models for
each pair of two devices. The default value is NULL which means that by default the models are initialized with the
steady state.}
}
\value{
a data.table object with two columns: 'deviceID' and 'dupP'. On the first column there are deviceIDs and on
 the second column the corresponding duplicity probability, i.e. the probability that a device is in a 2-to-1
 correspondence with its holder.
}
\description{
Computes the duplicity probability for each device using three different methods: pairs, 1-to-1 and
 trajectory. The theory behind these methods is described in detail in
 \href{https://webgate.ec.europa.eu/fpfis/mwikis/essnetbigdata/images/f/fb/WPI_Deliverable_I3_A_proposed_production_framework_with_mobile_network_data_2020_05_31_draft.pdf}{WPI
  Deliverable 3} and in the paper \emph{An end-to-end statistical process with mobile network data for Official
 Statistics}.
}
