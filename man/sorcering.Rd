\name{sorcering}
\alias{sorcering}
\docType{package}
\title{
  Soil ORganic Carbon & CN Ratio drIven Nitrogen modellinG framework
}
\author{
Marc Scherstjanoi \email{marc.scherstjanoi@thuenen.de}, Rene Dechow
}
\description{
\code{SORCERING} can be used to model the fate of soil organic carbon (SOC) and soil organic nitrogen (SON) and to calculate N mineralisation 
rates. 
It provides a framework that numerically solves differential equations of SOC models based on first-order kinetics.
Thus, SOC models can be simply defined and run to predict the temporal development of SOC. Beyond this, \code{SORCERING} determines the fluxes of SON and N mineralisation / immobilisation.
Basic inputs are
(1) the model parameters of a given SOC model expressed as the C transfer matrix (including information on decomposition and transfer rates between model pools),
(2) the initial distributions of C and N among model pools and
(3) time series of C and N inputs and rate modulating environmental factors.
The fourth-order Runge-Kutta algorithm is used to numerically solve the system of differential equations.
\loadmathjax  
}
\usage{
sorcering(    A = NULL,
              t_sim = 2,
              tsteps = "monthly",
              C0 = NULL,
              N0 = NULL,
              Cin = NULL,
              Nin = NULL,
              xi = NULL,
              calcN = FALSE,
              calcNbalance = FALSE)
}
\arguments{
  \item{A}{ transfer matrix. Defines number of pools, decomposition and transfer rates. Must be a square matrix. n\mjeqn{\times}{X}n elements with n = number of pools. Diagonal values are decomposition rates 
  [\mjeqn{yr^{-1}}{yr^(-1)}]. Off-diagonals represent the transfer between pools [\mjeqn{yr^{-1}}{yr^(-1)}].}
  \item{t_sim}{number of simulation time steps. Must correspond to the number of rows of \code{Cin}, \code{Nin} and \code{xi}.}
  \item{tsteps}{character indicating the type of simulation time steps. valid options are \code{annually}, \code{monthly} (recommended) or \code{weekly}.}
  \item{C0}{vector with a length equal to the number of pools. Contains initial soil organic carbon per pool [\mjeqn{t C \ ha^{-1}}{t C ha^(-1)}]. If \code{NULL}, filled with zeros.}
  \item{N0}{vector with a length equal to the number of pools. Contains initial soil organic nitrogen per pool [\mjeqn{t N \ ha^{-1}}{t N ha^(-1)}]. If \code{NULL}, filled with zeros. Only used when \code{calcN = TRUE}.}
  \item{Cin}{matrix with a number of columns equal to the number of pools and a number of rows corresponding to \code{t_sim}. Contains carbon input per pool and time step [\mjeqn{t C \ ha^{-1}}{t C ha^(-1)}]. If \code{NULL}, filled with zeros.}
  \item{Nin}{matrix with a number of columns equal to the number of pools and a number of rows corresponding to \code{t_sim}. Contains nitrogen input per pool and time step [\mjeqn{t N \ ha^{-1}}{t N ha^(-1)}]. If \code{NULL}, filled with zeros. Must be \code{>0} where \code{Cin>0}. Only used when \code{calcN = TRUE}.}
  \item{xi}{matrix with a number of columns equal to the number of pools and a number of rows corresponding to \code{t_sim}. Contains environmental factors. If \code{NULL}, filled with ones.}
  \item{calcN}{logical indicating whether the development of soil organic nitrogen should be simulated.}
  \item{calcNbalance}{logical indicating whether the balance of nitrogen cycling should be calculated.}
}
\details{
  \code{SORCERING} is a general model framework to describe soil organic carbon (SOC) dynamics and soil organic nitrogen (SON) dynamics based on models of first-order kinetics.
  It can be applied to any given SOC first-order kinetics model.
  The approach has already been successfully tested to describe SOC dynamics of Yasso \insertCite{Tuomi2009}{sorcering}, RothC \insertCite{Coleman1996}{sorcering} and
  C-Tool \insertCite{Taghizadeh-Toosi2014}{sorcering}.
  Therefore, \code{SORCERING} is a lightweight alternative to the widely developed and multifunctional R package SoilR \insertCite{Sierra2012,Sierra2014}{sorcering}.
  Moreover, it additionally offers the possibility of modelling N immobilisation and mineralisation by enhancing given SOC models by an additional N module.
  
  The following is a description of each element calculated, which also corresponds to the output values (see section 'Value').
  For a detailed mathematical description of the \code{SORCERING} function see the extended pdf documentation at \code{browseVignettes("sorcering")}.
\cr
\cr

  \strong{C}
  
  \code{SORCERING} calculates SOC applying a given SOC model for every simulation time step defined by \code{tsteps} and \code{t_sim}.
  SOC models applied here are defined by a number of pools, each characterised by specific decomposition and turnover rates.
  The model-specific decomposition kinetics and SOC fluxes among pools are described by a set of partial differential equations represented by the transfer matrix \code{A}. 
  Each row and column of \code{A} represent SOC pools. Off-diagonal elements of \code{A} describe SOC fluxes and diagonal elements describe SOC decomposition.
  The differential equations furthermore contain the boundary conditions \code{Cin} and \code{xi}. 
  The change of SOC concentration in time thus is defined as:
  
  \mjdeqn{\frac{ dC(t) }{ dt } = Cin(t)+A_e(t) \cdot C(t)}{dC(t)/dt = Cin(t) + A_e(t) * C(t)}
  
  with 
  \mjdeqn{A_e(t) = A \cdot diag(xi(t))}{Ae(t) = A * diag(xi(t))}
  
  Initial conditions must be defined for every SOC pool by \code{C0}.  
  A description of the numerical solution can be found in the extended pdf documentation at \code{browseVignettes("sorcering")}.
  For more information on the functioning and possibilities of solving first-order kinetics SOC models see \insertCite{Sierra2012;textual}{sorcering}.
\cr  
\cr

  \strong{N}
 
  As an extension to SOC modelling, \code{SORCERING} allows the modelling of SON coupled to the modelling of SOC. Its implementation is based on the following simplifying assumptions:
  (1) Nitrogen transfer and turnover rates are equal to carbon rates.
  (2) There is no N limitation in the soil, i.e. mineral N is always available for N immobilisation processes.
  (3) CN ratios of single pools are only affected by external inputs of N and C. The transfer of organic matter among pools does not affect CN ratios. 
  As for SOC, the development of SON depends on initial and boundary conditions:  \code{N0} and \code{Nin}.
  As N decomposition is proportional to C decomposition, SON is calculated based on the results of the SOC calculations and
  input conditions (for details see the extended pdf documentation at \code{browseVignettes("sorcering")}).
\cr  
\cr

\strong{Nloss, Nmin, Nmin.sink<1>, ..., Nmin.sink<n>}

  Along with modelling SON, further quantities are determined.
  Nitrogen losses are calculated as:
  
  \mjdeqn{ 
    Nloss(t) = N(t-1) + Nin(t-1) - N(t)     
  }{Nloss(t) = N(t-1) + Nin(t-1) - N(t) } 
  
  In contrast, mineralisation rates contain information about sources and sinks of SON.
  They are calculated based on the CN ratios in the pools and the turnover rates (for details see the extended pdf documentation at \code{browseVignettes("sorcering")}).
  Pool-specific N mineralisation \mjeqn{Nmin.sink\left\langle 1 \right\rangle, ..., Nmin.sink\left\langle n \right\rangle}{Nmin.sink_(1, ..., n)} and N mineralisation \mjeqn{Nmin}{Nmin} are related the following:
  
  \mjdeqn{ 
    Nmin_j(t) = \sum_{p=1}^{n} Nmin.sink \left\langle j \right\rangle_{p}(t)   
  }{Nmin_j(t) = sum over p=1..n of Nmin.sink<j>_p(t) }  
  
  for each simulation time point \mjeqn{t}{t}, each pool \mjeqn{j = 1, ..., n}{j = 1, ..., n} and each pool \mjeqn{p = 1, ..., n}{p = 1, ..., n} and \mjeqn{n}{n} total pools.
  Or in other words, the row sum of \mjeqn{Nmin.sink\left\langle j \right\rangle}{Nmin.sink<j>} at one simulation time point 
  equals the \ifelse{html}{\out{j<sup>th</sup>}}{\eqn{j^{th}}} column of \mjeqn{Nmin}{Nmin} at that time point.
  
  As changes in SON must match the sums of all mineralisation paths, the sums over soil pools of \code{Nloss} and \code{Nmin}, respectively, must be approximately equal for all simulation time points:
  
  \mjtdeqn{}{\begin{matrix} \sum_{p=1}^{n} Nloss_p(t) \approx \sum_{p=1}^{n} Nmin_p(t) & \forall t \in tseq \end{matrix}}{sum over p=1..n of Nloss_p(t) ~ sum over p=1..n of Nmin_p(t), for all t element t_seq}    
  \mjtdeqn{\sum_{p=1}^{n} Nloss_p(t) \approx \sum_{p=1}^{n} Nmin_p(t) , \forall t \in tseq }{}{}
 
  A verification of this relation is given by \code{"Nbalance"} (see below). 
\cr
\cr

  \strong{Nbalance}
  
  The overall N change between two time steps is calculated as:
  \mjdeqn{ 
    \Delta N (t) = \sum_{p=1}^{n} N_p(t-1) - \sum_{p=1}^{n} N_p(t)    
  }{dN(t) = sum over p=1..n of N_p(t) - sum over p=1..n of N_p(t-1) }          
  
  The total system N balance serves as a verification output. Both of the following equations should give results close to zero:
  \mjdeqn{ 
    N_{bal1}(t) = \sum_{p=1}^{n} Nin_p(t-1) + \Delta N (t) - \sum_{p=1}^{n} Nloss_p(t) \approx 0
  }{N_bal1(t) = sum over p=1..n of Nin_p(t-1) - dN(t) - sum of p=1..n of Nloss_p(t) ~ 0}           
  
  \mjdeqn{ 
    N_{bal2}(t) = \sum_{p=1}^{n} Nin_p(t-1) + \Delta N (t) - \sum_{p=1}^{n} Nmin_p(t) \approx 0  
  }{N_bal2(t) = sum over p=1..n of Nin_p(t-1) - dN(t) - sum of p=1..n of Nmin_p(t) ~ 0}   
  

}

\value{
 \code{sorcering()} returns a list object with components:
  \item{C}{array with a number of rows corresponding to \code{t_sim} and a number of columns equal to the number of pools. Contains soil organic carbon [\mjeqn{t C \ ha^{-1}}{t C ha^(-1)}].}
  \item{N}{array with a number of rows corresponding to \code{t_sim} and a number of columns equal to the number of pools. Contains soil organic nitrogen [\mjeqn{t N \ ha^{-1}}{t N ha^(-1)}]. Only generated if \code{calcN = TRUE}.}
  \item{Nloss}{array with a number of rows corresponding to \code{t_sim} and a number of columns equal to the number of pools. Contains nitrogen losses [\mjeqn{t N \ ha^{-1}}{t N ha^(-1)}]. Only generated if \code{calcN = TRUE}.}
  \item{Nmin}{array with a number of rows corresponding to \code{t_sim} and a number of columns equal to the number of pools. Contains nitrogen mineralisation [\mjeqn{t N \ ha^{-1}}{t N ha^(-1)}].  If values are negative, nitrogen immobilisation exceeds mineralisation. Only generated if \code{calcN = TRUE}.}
  \item{Nmin.sink.1, ..., Nmin.sink.n}{arrays with a number of rows corresponding to \code{t\_sim} and a number of columns equal to the number of pools \code{n}. Contain pool-specific nitrogen mineralisation sinks [\mjeqn{t N \ ha^{-1}}{t N ha^(-1)}] (from the pool according to variable index [1, ..., n] to the pool according to column number). If the sink is the pool itself (index equals column number) the amount of decomposition is recorded. Only generated if \code{calcN = TRUE}.}
  \item{Nbalance}{array with a number of rows corresponding to \code{t_sim} and three columns. Contains information on overall N changes in the soil between two time steps (first column) and information on total system N balance calculated based on total \code{Nloss} (second column) and based on total \code{Nmin} (third column) [\mjeqn{t N \ ha^{-1}}{t N ha^(-1)}]. Only generated if \code{calcN = TRUE} and \code{calcNbalance = TRUE}.}
}
  

\section{Package Building Information}{

The \code{SORCERING} code was written in C++ using the R packages \code{Rcpp} \insertCite{Eddelbuettel2021}{sorcering}
and \code{RcppArmadillo} \insertCite{Eddelbuettel2021a}{sorcering}. 
This documentation was built with the help of the R packages \code{mathjaxr} \insertCite{Viechtbauer2021}{sorcering}
and \code{Rdpack} \insertCite{Boshnakov2021}{sorcering}.
}


\references{

\insertAllCited{}

}

\examples{

  #EXAMPLE OF RothC application with fictional input

  #1. Input

  data(Cin_ex, Nin_ex, N0_ex, C0_ex, xi_ex) #fictional data
  A_RothC <- fget_A_RothC(clay=30) #create transfer matrix for RothC

  #2. simulation

  out <- sorcering(A=A_RothC, t_sim=60, Cin=Cin_ex, Nin=Nin_ex, 
  N0=N0_ex, C0=C0_ex, xi=xi_ex, calcN=TRUE, tsteps="monthly")
  
  #3. results

  #output structure summary
  summary(out)

  #sample plot
  oldpar <- par(no.readonly = TRUE) #save old par 
  par(mfrow=c(1,1),mar=c(4,5,2,5))
  plot(rowSums(out$N),axes=FALSE, col=1, cex.lab=1.5,xlab="",ylab="",ylim=c(0,9),pch=20)
  par(new=TRUE)
  plot(rowSums(Cin_ex)/rowSums(Nin_ex),
    axes=FALSE,col=2, cex.lab=1.5,xlab="",ylab="",ylim=c(0,60),pch=20)
  axis(side=2, pos = 0, labels = c((0:4)*1.5,"N",9), 
    at=(0:6)*10, hadj=1, padj = 0.5, cex.axis=1.5,las=1,col.axis=1)
  axis(side=4, pos = 60, labels = c((0:4)*10,"Cin/Nin",60),
    at=(0:6)*10, hadj=0, padj = 0.5, cex.axis=1.5, las=1,col.axis=2)
  axis(side=1, pos = 0, labels =  c((0:4)*10,"t",60),
    at=(0:6)*10, hadj=0.5, padj = 0, cex.axis=1.5)
  par(oldpar) #back to old par
}
