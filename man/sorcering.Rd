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
It provides a framework that numerically solves differential equations 
of SOC models based on first-order kinetics.
An SOC model can be simply defined or a predefined existing SOC model can be chosen and then run to predict the temporal development of SOC. Beyond this, \code{SORCERING} determines the fluxes of SON and N mineralisation / immobilisation.
Basic inputs are
(1) the model parameters of a given SOC model expressed as the C transfer matrix (including information on decomposition and transfer rates between model pools),
(2) either the initial distributions of C and N among model pools as a direct input or
time series of at least three C and N measurement points with which these initial distributions
can be calculated using linear regression,
and
(3) time series of C and N inputs and rate modifying environmental factors.
In case a predefined SOC model is used, instead of model parameters and time series of rate modifying factors, 
model-specific environmental and stand data must be passed for the calculation of decomposition and transfer rates.
The fourth-order Runge-Kutta algorithm is used to numerically solve the system of differential equations.
\loadmathjax  
}
\usage{
sorcering(    A = NULL,
              tsteps = "monthly",
              t_sim = 2,
              C0 = NULL,
              N0 = NULL,
              Cin = NULL,
              Nin = NULL,
              Cin_wood = NULL,
              Nin_wood = NULL,
              wood_diam = NULL,
              xi = NULL,
              env_in = NULL,
              site = NULL,
              theta = NULL,
              theta_unc = NULL,
              theta_n_unc = 1,
              meas_data = NULL,
              t_sim_sl = 2,
              A_sl = NULL,
              C0_sl = NULL,
              N0_sl = NULL,
              Cin_sl = NULL,
              Nin_sl = NULL,
              Cin_wood_sl = NULL,
              Nin_wood_sl = NULL,
              wood_diam_sl = NULL,
              xi_sl = NULL,
              env_in_sl = NULL,
              site_sl = NULL,
              sitelist = NULL,        
              meas_data_sl = NULL,
              calcN = FALSE,
              calcNbalance = FALSE,
              calcN0 = FALSE,
              calcC0 = FALSE,
              calcCN_fast_init = FALSE,
              CTool_input_raw = FALSE,
              RothC_Cin4C0 = FALSE,
              C0_fracts = NULL,
              multisite = FALSE,
              pooltypes = NULL,
              CN_fast_init = 40,
              CN_bio = 9,
              CN_fast_init_sl = NULL,
              CN_bio_sl = NULL,
              init_info = FALSE,
              model = "")
}
\arguments{
  \item{A}{ square matrix. Transfer matrix typical for SOC modelling. Defines number of pools, decomposition and transfer rates. n\mjeqn{\times}{x}n elements with n = number of pools. Diagonal values are decomposition rates [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]. Off-diagonals represent the transfer between pools . Only used when \code{model is NULL}}
  \item{tsteps}{character string indicating the type of simulation time steps. Valid options are \code{"annually"}, \code{"monthly"} (recommended) or \code{"weekly"}. Ensures that the rate modifying factors (passed through \code{xi}
 or used by a predefined model) are adjusted by dividing them by 1, 12, and 52 respectively. Also ensures that environment-specific information (passed through \code{env_in}) takes into account the time reference of the modelling.}
  \item{t_sim}{integer number of simulation time steps. Must correspond to the number of rows of \code{Cin}, \code{Nin} and \code{xi}.}
  \item{C0}{either vector with a length equal to the number of pools or scalar. If vector, initial soil organic carbon per pool [\ifelse{html}{\out{tC ha<sup>-1</sup>}}{\eqn{\mathrm{tC \, ha^{-1}}}}]. 
  If scalar, initial total soil organic carbon [\ifelse{html}{\out{tC ha<sup>-1</sup>}}{\eqn{\mathrm{tC \, ha^{-1}}}}]. In the latter case, either \code{model} must be selected or \code{C0_fracts} must be passed. 
  If \code{NULL}, filled with zeros.}
  \item{N0}{vector with a length equal to the number of pools. Contains initial soil organic nitrogen per pool [\ifelse{html}{\out{tN ha<sup>-1</sup>}}{\eqn{\mathrm{tN \, ha^{-1}}}}]. If \code{NULL}, filled with zeros. Only used when \code{calcN = TRUE} and \code{calcN0 = FALSE}.}
  \item{Cin}{either matrix with a number of columns equal to the number of pools and a number of rows corresponding to \code{t_sim}, or list containing such matrices. If it is a list, each element of the list is expected to represent a stochastic repetition that covers input uncertainties. Then, the list must contain matrices of equal dimensions. Each matrix (or the one if modelling without uncertainties) must contain information about carbon input per pool and time step [\ifelse{html}{\out{tC ha<sup>-1</sup>}}{\eqn{\mathrm{tC \, ha^{-1}}}}]. When \code{CTool_input_raw = TRUE}, and \code{model = "C-Tool"} or \code{model = "C-Tool-org"}, the matrix structure can have two columns (as described for \code{CTool_input_raw}). If \code{NULL}, filled with zeros.}
  \item{Nin}{either matrix with a number of columns equal to the number of pools and a number of rows corresponding to \code{t_sim}, or list containing such matrices. If it is a list, each element of the list is expected to represent a stochastic repetition that covers input uncertainties. Then, the list must contain matrices of equal dimensions. Each matrix (or the one if modelling without uncertainties) must contain information about nitrogen input per pool and time step [\ifelse{html}{\out{tN ha<sup>-1</sup>}}{\eqn{\mathrm{tN \, ha^{-1}}}}]. When \code{CTool_input_raw = TRUE}, and \code{model = "C-Tool"} or \code{model = "C-Tool-org"} the matrix structure can have 2 columns (as described for \code{CTool_input_raw}). If \code{NULL}, filled with zeros. Must contain entries \code{> 0} where entries of \code{Cin} are \code{> 0}. Only used when \code{calcN = TRUE}.}
  \item{Cin_wood}{list of lengths of different wood diameter classes. Each list element must be in \code{Cin} format and represent a specific wood diameter. Furthermore, the list elements themselves can be lists and contain stochastic repetitions, as explained for \code{Cin}. The mean diameter per class is defined in \code{wood_diam}. Only used when \code{model = "Yasso15"} or \code{model = "Yasso20"}.}
  \item{Nin_wood}{list of lengths of different wood diameter classes. Each list element must be in \code{Nin} format and represent a specific wood diameter. Furthermore, the list elements themselves can be lists and contain stochastic repetitions, as explained for \code{Nin}. The mean diameter per class is defined in \code{wood_diam}. Must contain entries \code{> 0} where entries of \code{Cin_wood} are \code{> 0}. Only used when \code{calcN = TRUE}. Only used when \code{model = "Yasso15"} or \code{model = "Yasso20"}. }
  \item{wood_diam}{ vector with wood diameter [cm]. The first element corresponds to the first list element of \code{Cin_wood} and \code{Nin_wood}. If \code{NULL}, filled with zeros. Only used when \code{Cin_wood} is specified and when either \code{model = "Yasso15"} or \code{model = "Yasso20"}. Must contain entries \code{>= 0}.}
  \item{xi}{either matrix with a number of columns equal to the number of pools and a number of rows corresponding to \code{t_sim} or list containing such matrices. If it is a list, each element of the list is expected to represent a stochastic repetition that covers input uncertainties. Then, the list must contain matrices of equal dimensions. Each matrix (or the one if modelling without uncertainties) must contain information about time series of rate modifying factors for each model pool, built on the basis of annual decomposition rates. If \code{NULL}, filled with ones. Only used when \code{model is NULL}.}
  \item{env_in}{ matrix with a model-specific number of columns and a number of rows corresponding to \code{t_sim}. Contains environment-specific information to calculate rate modifying factors (instead of passing them with \code{xi}) and initial distributions (only RothC). When \code{model = "RothC"}, it must have four columns: atmospheric temperature (T) [degrees C], precipitation (p) [mm], evapotranspiration [mm] and a vector of zeros and ones, where ones indicate time steps when the soil is vegetated and zeros when it is bare. When \code{model = "Yasso07"} or \code{model = "Yasso15"} or \code{model = "Yasso20"}, it must have two columns: T [degrees C] and p [mm]. When \code{model = "C-Tool"} or \code{model = "C-Tool-org"}, it has one column: T [degrees C]. If \code{NULL}, filled with ones. Only used when \code{model is not NULL}.}
  \item{site}{vector of model-specific length. Contains site-specific information to calculate rate modifying factors (instead of passing them with \code{xi}) and initial distributions (only RothC). and initial carbon and nitrogen distributions. When \code{model = "RothC"}, it must be of length four: sample depth [mm], clay content [\ifelse{html}{\out{\%}}{\%}], black sand status (0 or 1, 0 if unknown or if black sand method is not desired) and CN ratio (0 if unknown, but then either \code{C0} and \code{N0} must be passed or \code{calcC0 = TRUE} and \code{calcN0 = TRUE}, information on CN ratio given in \code{site} always takes precedence over internally calculated CN ratios). When \code{model = "C-Tool"} or \code{model = "C-Tool-org"}, it must be of length one: clay content [\ifelse{html}{\out{\%}}{\%}]. Only used when \code{model = "RothC"} or \code{model = "C-Tool"} or \code{model = "C-Tool-org"}.}
  \item{theta}{
  either vector with model parameters for predefined models or matrix with rows of such parameters. If it is a matrix, each row is expected to represent a stochastic repetition that covers input uncertainties. If uncertainties are defined by another argument, e.g. \code{Cin} or \code{Nin}, these determine the number of stochastic repetitions and not \code{theta}. Then, if \code{theta} is a matrix, a parameter vector is randomly drawn for each uncertainty loop. Each vector (or row of matrix) must be of length 7 when \code{model = "RothC"}, of length 10 when \code{model = "C-Tool"} or \code{model = "C-Tool-org"}, of length 21 when \code{model = "Yasso07"} and of length 30 when \code{model = "Yasso15"} or \code{model = "Yasso20"}. If \code{NULL}, model-specific standard parameters are used instead. Only used when \code{model is not NULL}. See model parameters table in section 'Details' for standard parameters used. }  
  \item{theta_unc}{either number or vector of percentage values. If it is a vector, the same model-specific lengths as described for \code{theta} must be used. When used, model parameters modified by taking from the normal distribution around given values (either from \code{theta} or predefined values) with a standard deviation of \code{theta_unc}. This will be repeated as many times as defined in \code{theta_n_unc} or as defined by uncertainty dimensions of a carbon or nitrogen input argument (e.g. \code{Cin}) and lead to unique model results and output list elements.  Only used when \code{model is not NULL} and \code{theta} is not a matrix.}
  \item{theta_n_unc}{number of stochastic repetitions when model parameters for predefined models should be determined from a random distribution. Only used when the number of stochastic repetitions is not defined by another argument (e.g. \code{Cin} ). Only used when \code{model is not NULL}, \code{theta_unc is not NULL} and \code{theta} is not a matrix.}  
  \item{meas_data}{ matrix with a number of rows equal to the number of measurement points. The first column defines the time of measurement, the metric of which is based on simulation time steps. The second row must contain values of measured soil organic carbon stock. The third row must contain values of measured soil organic nitrogen and is only used when \code{calcN0 = TRUE}. Only used when \code{calcC0 = TRUE}.}
  \item{t_sim_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{t_sim} format. Only used when \code{multisite = TRUE}.}
  \item{A_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{A} format. Only used when \code{multisite = TRUE} and \code{model is NULL}. When \code{multisite = TRUE}, \code{A} can be passed instead of \code{A_sl} to have the same argument for all sites. }
  \item{C0_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{C0} format. Only used when \code{multisite = TRUE}.}
  \item{N0_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{N0} format. Only used when \code{multisite = TRUE}, \code{calcN = TRUE} and \code{calcN0 = FALSE}.}
  \item{Cin_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{Cin} format, which can also contain uncertainties. Thus, \code{Cin_sl} can either be a list of different sites each containing lists of different uncertainty representations each with matrices of carbon input as described for \code{Cin}, or it can simply be a list of different sites each containing such matrices.  
  Only used when \code{multisite = TRUE}.}
  \item{Nin_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{Nin} format, which can also contain uncertainties. Thus, \code{Nin_sl} can either be a list of different sites each containing lists of different uncertainty representations each with matrices of carbon input as described for \code{Nin}, or it can simply be a list of different sites each containing such matrices.  
  Only used when \code{multisite = TRUE}. Must contain entries \code{> 0} where entries of \code{Cin_sl} are \code{> 0}.}
  \item{Cin_wood_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{Cin_wood} format, which can also contain uncertainties. 
  Thus, \code{Cin_wood_sl} can either be a list of different sites each containing lists of different wood diameter representations, which in turn each contain lists of different uncertainty representations, each with matrices of carbon input as described for \code{Cin}, or it can simply be a list of different sites each containing lists of different wood diameter representations each containing such matrices.   
  Only used when \code{multisite = TRUE} and either \code{model = "Yasso15"} or \code{model = "Yasso20"}.}
  \item{Nin_wood_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{Nin_wood} format, which can also contain uncertainties. 
  Thus, \code{Nin_wood_sl} can either be a list of different sites each containing lists of different wood diameter representations, which in turn each contain lists of different uncertainty representations, each with matrices of carbon input as described for \code{Nin}, or it can simply be a list of different sites each containing lists of different wood diameter representations each containing such matrices.   
  Only used when \code{multisite = TRUE} and either \code{model = "Yasso15"} or \code{model = "Yasso20"}. Must contain entries \code{> 0} where entries of \code{Cin_wood_sl} are \code{> 0}.}
  \item{wood_diam_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{wood_diam} format. Only used when \code{multisite = TRUE} and either \code{model = "Yasso15"} or \code{model = "Yasso20"}.}
  \item{xi_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{xi} format, which can also contain uncertainties. In the latter case, the site list must include uncertainty lists. Only used when \code{multisite = TRUE}.}
  \item{env_in_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{env_in} format. Only used when \code{multisite = TRUE}.}
  \item{site_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{site} format. Only used when \code{multisite = TRUE}.}
  \item{sitelist}{list with names of sites to simulate. Only used when \code{multisite = TRUE}.}
  \item{meas_data_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{meas_data} format. Consequently, it is only used when \code{calcC0 = TRUE} and the third row of each list element is only used when \code{calcN0 = TRUE}. Only used when \code{multisite = TRUE}.}
  \item{calcN}{logical indicating whether soil organic nitrogen should be modeled.}
  \item{calcNbalance}{logical indicating whether the balance of nitrogen cycling should be calculated.}
  \item{calcN0}{logical indicating whether \code{N0} should be calculated. Then, the information in \code{meas_data} is used to determine initial states using linear regression.}
  \item{calcC0}{logical indicating whether \code{C0} should be calculated. Then, the information in \code{meas_data} is used to determine initial states using linear regression.}
  \item{calcCN_fast_init}{ logical indicating whether to calculate the initial CN ratio for fast pools (using \code{Cin} and \code{Nin}) or whether \code{CN_fast_init} should be used.}
  \item{CTool_input_raw}{ logical defining of which type \code{Cin} and \code{Nin} are when modelling with \code{C-Tool}. If \code{true}, 
 \code{Cin} and \code{Nin} can only have two columns, one for the topsoil and one for the subsoil, and \code{SORCERING} is applying the \code{C-Tool}-specific distribution to model pools.
 If \code{false} (default) \code{Cin} and \code{Nin} must have six columns, one per model pool.
 Only used when \code{model = "C-Tool"} or \code{model = "C-Tool-org"}.}
  \item{RothC_Cin4C0}{ logical defining whether the SOC input should be used for the calculation of initial SOC. If \code{false} the standard RothC ratio for agricultural soils of DPM to RPM of 0.59 to 0.41 is used.
 Only used when \code{model = "RothC"} and \code{calcC0 = TRUE}.} 
  \item{C0_fracts}{numerical vector of a length equal to the number of pools. Contains initial fractions of SOC in pools, the sum of which must be 1. Only used when \code{calcC0 = TRUE}.}
  \item{multisite}{ logical indicating whether multiple sites should be calculated with one program call. Then, \code{t_sim}, \code{C0}, \code{N0}, \code{Cin}, \code{Nin}, \code{Cin_wood}, \code{Nin_wood}, \code{wood_diam}, \code{env_in}, \code{site_in}, \code{xi} and \code{meas_data} must be of list type and replaced with \code{t_sim_sl}, \code{C0_sl}, \code{N0_sl}, \code{Cin_sl}, \code{Nin_sl}, \code{Cin_wood_sl}, \code{Nin_wood_sl}, \code{wood_diam_sl}, \code{env_in_sl}, \code{site_in_sl}, \code{xi_sl} and \code{meas_data_sl}, respectively. \code{A}, \code{CN_bio} and \code{CN_fast_init} can be given as single variables or in list form of \code{A_sl}, \code{CN_bio_sl} and \code{CN_fast_init_sl}, respectively.}
  \item{pooltypes}{integer vector with a length equal to the number of pools. Contains information necessary for the calculation of \code{N0}. Allowed values are 1-6. 1: topsoil fast pool, 2: topsoil bio or humus pool, 3: topsoil chemically stable or inert pool, 4: subsoil fast pool, 5: subsoil bio or humus pool, 6: subsoil chemically stable or inert pool. Predefined values are (1,1,2,2,3) when \code{model = "RothC"}, (1,2,3,4,5,6) when \code{model = "C-Tool"} or \code{model = "C-Tool-org"}, (1,1,1,2,3) when \code{model = "Yasso07"} or \code{model = "Yasso15"} or \code{model = "Yasso20"}. Only used when \code{calcN = TRUE} and \code{calcN0 = TRUE}.}
  \item{CN_fast_init}{number that defines the initial CN ratio for fast pools (\code{pooltypes = 1 or 4}). Only used when \code{Nin} (or \code{Nin_sl}) and \code{Cin} (or \code{Cin_sl}) do not provide enough information for the estimation of initial nitrogen. The user will be informed about it when \code{init_info = TRUE}. Only used when \code{calcN = TRUE} and \code{calcN0 = TRUE}.}
  \item{CN_bio}{number that defines the initial CN ratio for slow pools (\code{pooltypes = 2 or 5}). Only used when \code{calcN = TRUE} and \code{calcN0 = TRUE}.}
  \item{CN_fast_init_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{CN_fast_init} format. Only used when \code{calcN = TRUE}, \code{multisite = TRUE} and \code{calcN0 = TRUE}. When \code{multisite = TRUE}, \code{CN_fast_init} can be passed instead of \code{CN_fast_init_sl} to have the same argument for all sites.}
  \item{CN_bio_sl}{list with a length of number of sites to simulate. Each list element represents a site and must be in \code{CN_bio} format. Only used when \code{calcN = TRUE}, \code{multisite = TRUE} and \code{calcN0 = TRUE}. When \code{multisite = TRUE}, \code{CN_bio} can be passed instead of \code{CN_bio_sl} to have the same argument for all sites.}
  \item{init_info}{logical indicating whether additional information about the calculation of initial carbon, initial nitrogen, and CN ratio should be printed out during the simulations. Only used when \code{calcC0 = TRUE} or \code{calcN0 = TRUE}. }
  \item{model}{character string specifying a predefined soil organic carbon model to use. Valid options are \code{"Yasso07"}, \code{"Yasso15"}, \code{"Yasso20"}, \code{"RothC"}, \code{"C-Tool"} or \code{"C-Tool-org"}. When \code{not NULL}, \code{xi} and \code{A} are calculated by \code{SORCERING}, and \code{env_in} must be specified. Additionally, \code{theta} can be specified to not use standard model parameters. See model parameters table in section 'Details' for standard parameters used. If \code{calcN0 = TRUE} and \code{pooltypes = NULL} model-specific standard values for \code{pooltypes} are used.}
}
\details{
  \code{SORCERING} is a general model framework to describe soil organic carbon (SOC) dynamics and soil organic nitrogen (SON) dynamics based on models of first-order kinetics.
  It can be applied to any given SOC first-order kinetics model.
  The approach has already been successfully tested to describe SOC dynamics of Yasso \insertCite{Tuomi2009,Viskari2020,Viskari2022}{sorcering}, RothC \insertCite{Coleman1996}{sorcering} and
  C-Tool \insertCite{Taghizadeh-Toosi2014}{sorcering}.
  Moreover, it additionally offers the possibility of modelling N immobilisation and mineralisation by enhancing given SOC models by an additional N module.
  \code{SORCERING} was created using the C++ interface \code{Rcpp} \insertCite{Eddelbuettel2021}{sorcering} and can handle multiple sites and multiple stochastic representations with just one function call. This makes \code{SORCERING} a computationally efficient SOC and SON modelling tool.
 
  In the following a description of each output value (see section 'Value') is given.
  Detailed mathematical descriptions of the SOC and SON calculation, the optional extensions of the \code{SORCERING} function and the predefined models used can be found in the extended R documentation at
  \code{browseVignettes("sorcering")}.
\cr
\cr

  \strong{Value C}
  
  \code{SORCERING} calculates SOC applying a given SOC model for every simulation time step defined by passing \code{tsteps} and \code{t_sim}.
  SOC models applied here are defined by a number of pools, each characterised by specific decomposition and turnover rates.
  The model-specific decomposition kinetics and SOC fluxes among pools are described by a set of partial differential equations represented by the transfer matrix 
  \mjeqn{A}{A} (as passed with \code{A} or provided by \code{model}). 
  Each row and column of \mjeqn{A}{A} represent SOC pools. Off-diagonal elements of \mjeqn{A}{A} describe SOC fluxes and diagonal elements describe SOC decomposition.
  The differential equations furthermore contain the boundary condition \mjeqn{Cin(t)}{Cin(t)} (as passed with \code{Cin}) and the model-specific generated rate modifying factor series \mjeqn{xi(t)}{xi(t)} 
  (as passed with \code{xi} or calculated for a predefined \code{model}). 
  The change of SOC concentration in time is then defined as:
  
  \mjdeqn{\frac{ dC(t) }{ dt } = Cin(t)+A_e(t) \cdot C(t)}{dC(t)/dt = Cin(t) + A_e(t) * C(t)}
  
  with 
  \mjdeqn{A_e(t) = A \cdot diag(xi(t))}{Ae(t) = A * diag(xi(t))}
  
  Initial conditions must be defined for every SOC pool by passing \code{C0} or by using the capabilities of \code{SORCERING} to calculate it.  
  A description of the numerical solution can be found in the extended pdf documentation at \code{browseVignettes("sorcering")}.
  For more information on the functioning and possibilities of solving first-order kinetics SOC models see \insertCite{Sierra2012;textual}{sorcering}.
\cr  
\cr

  \strong{Value N}
 
  As an extension to SOC modelling, \code{SORCERING} allows the modelling of SON coupled to the modelling of SOC. Its implementation is based on the following simplifying assumptions:
  (1) Nitrogen transfer and turnover rates are equal to carbon rates.
  (2) There is no N limitation in the soil, i.e. mineral N is always available for N immobilisation processes.
  (3) CN ratios of single pools are only affected by external inputs of N and C. The transfer of organic matter among pools does not affect CN ratios. 
  As for SOC, the development of SON depends on initial and boundary conditions.
  As N decomposition is proportional to C decomposition, SON is calculated based on the results of the SOC calculations and
  input conditions (for details see the extended pdf documentation at \code{browseVignettes("sorcering")}).
\cr  
\cr

\strong{Values Nloss, Nmin, Nmin.sink<1>, ..., Nmin.sink<n>}

  Along with modelling SON, further quantities are determined.
  Nitrogen losses are calculated as:
  
  \mjdeqn{ 
    Nloss(t) = N(t-1) + Nin(t-1) - N(t)     
  }{Nloss(t) = N(t-1) + Nin(t-1) - N(t) } 
  
  In contrast, mineralisation rates contain information about sources and sinks of SON.
  They are calculated based on the CN ratios in the pools and the turnover rates (for details see the extended pdf documentation at \code{browseVignettes("sorcering")}).
  Pool-specific N mineralisation \mjeqn{Nmin.sink\left\langle 1 \right\rangle, ..., Nmin.sink\left\langle n \right\rangle}{Nmin.sink_(1, ..., n)} and N mineralisation \mjeqn{Nmin}{Nmin} are related as follows:
  
  \mjdeqn{ 
    Nmin_j(t) = \sum_{p=1}^{n} Nmin.sink \left\langle j \right\rangle_{p}(t)   
  }{Nmin_j(t) = sum over p=1..n of Nmin.sink<j>_p(t) }  
  
  for each simulation time point \mjeqn{t}{t}, each pool \mjeqn{j = 1, ..., n}{j = 1, ..., n} and each pool \mjeqn{p = 1, ..., n}{p = 1, ..., n} and \mjeqn{n}{n} total pools.
  Or in other words, the row sum of \mjeqn{Nmin.sink\left\langle j \right\rangle}{Nmin.sink<j>} at one simulation time point 
  equals the \ifelse{html}{\out{j<sup>th</sup>}}{\eqn{\mathrm{j^{th}}}} column of \mjeqn{Nmin}{Nmin} at that time point.
  
  As changes in SON must match the sums of all mineralisation paths, the sums over soil pools of \code{Nloss} and \code{Nmin}, respectively, must be approximately equal for all simulation time points:
  
  \mjtdeqn{}{\begin{matrix} \sum_{p=1}^{n} Nloss_p(t) \approx \sum_{p=1}^{n} Nmin_p(t)\end{matrix}}{sum over p=1..n of Nloss_p(t) ~ sum over p=1..n of Nmin_p(t)}    
  \mjtdeqn{\sum_{p=1}^{n} Nloss_p(t) \approx \sum_{p=1}^{n} Nmin_p(t)}{}{}
 
  A verification of this relation is given by \code{"Nbalance"} (see below). 
\cr
\cr

  \strong{Value Nbalance}
  
  The overall N change between two time steps is calculated as:
  \mjdeqn{ 
    \Delta N (t) = \sum_{p=1}^{n} N_p(t-1) - \sum_{p=1}^{n} N_p(t)    
  }{dN(t) = sum over p=1..n of N_p(t) - sum over p=1..n of N_p(t-1) }          
  
  The total system N balance serves as a verification output. Both of the following equations should always give results close to zero:
  \mjdeqn{ 
    N_{bal1}(t) = \sum_{p=1}^{n} Nin_p(t-1) + \Delta N (t) - \sum_{p=1}^{n} Nloss_p(t) \approx 0
  }{N_bal1(t) = sum over p=1..n of Nin_p(t-1) - dN(t) - sum of p=1..n of Nloss_p(t) ~ 0}           
  
  \mjdeqn{ 
    N_{bal2}(t) = \sum_{p=1}^{n} Nin_p(t-1) + \Delta N (t) - \sum_{p=1}^{n} Nmin_p(t) \approx 0  
  }{N_bal2(t) = sum over p=1..n of Nin_p(t-1) - dN(t) - sum of p=1..n of Nmin_p(t) ~ 0}   
  
  \mjeqn{\Delta N (t)}{dN (t)} is saved in the first column, \mjeqn{N_{bal1}(t)}{N_(bal1)(t)} in the second and \mjeqn{N_{bal2}(t)}{N_(bal2)(t)} in the third column of \code{"Nbalance"}.

  \strong{Model parameters}
  
  If a predefined model has been specified (\code{model is not NULL}) the following standard parameters are used.
  They can be changed using \code{theta} within the program call.
  
  \tabular{llcccl}{
    \bold{RothC} \tab    \tab  \tab \cr
            \tab k_dpm      \tab 10   \tab \tab \tab Decomposition rate for DPM pool [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab k_rpm      \tab 0.3  \tab \tab \tab Decomposition rate for RPM pool [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab k_bio      \tab 0.66 \tab \tab \tab Decomposition rate for BIO pool [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab k_hum      \tab 0.02 \tab \tab \tab Decomposition rate for HUM pool [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab k_iom      \tab 0    \tab \tab \tab Decomposition rate for IOM pool [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab R_W_max   \tab 1    \tab \tab \tab Maximum rate modifying factor for soil moisture\cr
            \tab R_W_min   \tab 0.2  \tab \tab \tab Minimum rate modifying factor for soil moisture\cr
          \cr
          \cr
          \cr
   \bold{C-Tool}   \tab  \tab  \tab \cr
            \tab            \tab C-Tool     \tab C-Tool-org \tab \tab \cr
            \tab k_fom_t    \tab 1.44       \tab 1.44       \tab \tab Decomposition rate for FOM pool (topsoil) [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab k_hum_t    \tab 0.0336     \tab 0.0336     \tab \tab Decomposition rate for HUM pool (topsoil) [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab k_rom_t    \tab 0.000463   \tab 0          \tab \tab Decomposition rate for ROM pool (topsoil) [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab k_fom_s    \tab 1.44       \tab 1.44       \tab \tab Decomposition rate for FOM pool (subsoil) [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab k_hum_s    \tab 0.0336     \tab 0.0336     \tab \tab Decomposition rate for HUM pool (subsoil) [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab k_rom_s    \tab 0.000463   \tab 0          \tab \tab Decomposition rate for ROM pool (subsoil) [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab tf         \tab 0.03       \tab 0          \tab \tab Fraction going to downward transport\cr
            \tab f_co2      \tab 0.628      \tab 0.628      \tab \tab Fraction of \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{\mathrm{CO_2}}} released\cr
            \tab f_rom      \tab 0.012      \tab 0          \tab \tab Fraction of fresh organic matter going to ROM pool\cr
            \tab f_hum      \tab 0          \tab 0.358      \tab \tab Fraction of input going to HUM pool\cr
          \cr
          \cr
          \cr
    \bold{Yasso}   \tab    \tab    \tab \cr
            \tab            \tab Yasso07    \tab Yasso15       \tab Yasso20        \tab \cr
            \tab kA         \tab 0.66       \tab 0.49       \tab 0.51           \tab Base decomposition rate for pool A [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab kW         \tab 4.3        \tab 4.9        \tab 5.19           \tab Base decomposition rate for pool W [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab kE         \tab 0.35       \tab 0.25       \tab 0.13           \tab Base decomposition rate for pool E [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab kN         \tab 0.22       \tab 0.095      \tab 0.1            \tab Base decomposition rate for pool N [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab kH         \tab 0.0033     \tab 0.0013     \tab 0.0015         \tab Base decomposition rate for pool H [\ifelse{html}{\out{yr<sup>-1</sup>}}{\eqn{\mathrm{yr^{-1}}}}]\cr
            \tab p1         \tab 0.32       \tab 0.44       \tab 0.5            \tab Transference fraction from pool A to pool W\cr   
            \tab p2         \tab 0.01       \tab 0.25       \tab 0              \tab Transference fraction from pool A to pool E\cr
            \tab p3         \tab 0.93       \tab 0.92       \tab 1              \tab Transference fraction from pool A to pool N\cr
            \tab p4         \tab 0.34       \tab 0.99       \tab 1              \tab Transference fraction from pool W to pool A\cr
            \tab p5         \tab 0          \tab 0.084      \tab 0.99           \tab Transference fraction from pool W to pool E\cr
            \tab p6         \tab 0          \tab 0.011      \tab 0              \tab Transference fraction from pool W to pool N\cr   
            \tab p7         \tab 0          \tab 0.00061    \tab 0              \tab Transference fraction from pool E to pool A\cr
            \tab p8         \tab 0          \tab 0.00048    \tab 0              \tab Transference fraction from pool E to pool W\cr
            \tab p9         \tab 0.01       \tab 0.066      \tab 0              \tab Transference fraction from pool E to pool N\cr
            \tab p10        \tab 0          \tab 0.00077    \tab 0              \tab Transference fraction from pool N to pool A\cr
            \tab p11        \tab 0          \tab 0.1        \tab 0.163          \tab Transference fraction from pool N to pool W\cr    
            \tab p12        \tab 0.02       \tab 0.65       \tab 0              \tab Transference fraction from pool N to pool E\cr
            \tab pH         \tab 0.04       \tab 0.0046     \tab 0.0015         \tab Transference fraction from AWEN pools to pool H\cr
            \tab beta_1     \tab 0.076      \tab 0.091      \tab 0.158          \tab \ifelse{html}{\out{1<sup>st</sup>}}{\eqn{\mathrm{1^{st}}}}-order temperature parameter for AWE pools [\ifelse{html}{\out{degrees C<sup>-1</sup>}}{\eqn{\mathrm{degrees \, C^{-1}}}}]\cr
            \tab beta_2     \tab -0.00089   \tab -0.00021   \tab -0.002         \tab \ifelse{html}{\out{2<sup>nd</sup>}}{\eqn{\mathrm{2^{nd}}}}-order temperature parameter for AWE pools [\ifelse{html}{\out{degrees C<sup>-2</sup>}}{\eqn{\mathrm{degrees \, C^{-2}}}}]\cr
            \tab beta_N1    \tab -          \tab 0.049      \tab 0.17           \tab \ifelse{html}{\out{1<sup>st</sup>}}{\eqn{\mathrm{1^{st}}}}-order temperature parameter for N pool [\ifelse{html}{\out{degrees C<sup>-1</sup>}}{\eqn{\mathrm{degrees \, C^{-1}}}}]\cr    
            \tab beta_N2    \tab -          \tab -0.000079  \tab -0.005         \tab \ifelse{html}{\out{2<sup>nd</sup>}}{\eqn{\mathrm{2^{nd}}}}-order temperature parameter for N pool [\ifelse{html}{\out{degrees C<sup>-2</sup>}}{\eqn{\mathrm{degrees \, C^{-2}}}}]\cr
            \tab beta_H1    \tab -          \tab 0.035      \tab 0.067          \tab \ifelse{html}{\out{1<sup>st</sup>}}{\eqn{\mathrm{1^{st}}}}-order temperature parameter for H pool [\ifelse{html}{\out{degrees C<sup>-1</sup>}}{\eqn{\mathrm{degrees \, C^{-1}}}}]\cr
            \tab beta_H2    \tab -          \tab -0.00021   \tab 0              \tab \ifelse{html}{\out{2<sup>nd</sup>}}{\eqn{\mathrm{2^{nd}}}}-order temperature parameter for H pool [\ifelse{html}{\out{degrees C<sup>-2</sup>}}{\eqn{\mathrm{degrees \, C^{-2}}}}]\cr
            \tab gamma      \tab -1.27      \tab -1.8       \tab -1.44          \tab Precipitation impact parameter for AWE pools [\ifelse{html}{\out{yr mm<sup>-1</sup>}}{\eqn{\mathrm{yr \, mm^{-1}}}}]\cr
            \tab gamma_N    \tab -          \tab -1.2       \tab -2             \tab Precipitation impact parameter for N pool [\ifelse{html}{\out{yr mm<sup>-1</sup>}}{\eqn{\mathrm{yr \, mm^{-1}}}}]\cr
            \tab gamma_H    \tab -          \tab -13        \tab -6.9           \tab Precipitation impact parameter for H pool [\ifelse{html}{\out{yr mm<sup>-1</sup>}}{\eqn{\mathrm{yr \, mm^{-1}}}}]\cr
            \tab theta_1    \tab -          \tab -0.44      \tab -2.55          \tab \ifelse{html}{\out{1<sup>st</sup>}}{\eqn{\mathrm{1^{st}}}}-order impact parameter for wood size [\ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{\mathrm{cm^{-1}}}}]\cr
            \tab theta_2    \tab -          \tab 1.3        \tab 1.24           \tab \ifelse{html}{\out{2<sup>nd</sup>}}{\eqn{\mathrm{2^{nd}}}}-order impact parameter for wood size [\ifelse{html}{\out{cm<sup>-2</sup>}}{\eqn{\mathrm{cm^{-2}}}}]\cr
            \tab r          \tab -          \tab 0.26       \tab 0.25           \tab Exponent parameter for wood size
  }  
}

\value{
\code{SORCERING} returns either a list of carbon and nitrogen output values or, when \code{multisite = TRUE}, a list broken down by site with result lists for each site.
When modelling uncertainties (as can be defined by passing e.g. \code{Cin}, \code{Nin}, \code{xi} or \code{theta}), 
the output is even extended to include another list dimension that covers these uncertainties.
The lowest output list-level contains the following components:
  \item{C}{matrix with a number of rows corresponding to \code{t_sim} and a number of columns equal to the number of pools. Contains soil organic carbon [\ifelse{html}{\out{tC ha<sup>-1</sup>}}{\eqn{\mathrm{tC \, ha^{-1}}}}].}
  \item{N}{matrix with a number of rows corresponding to \code{t_sim} and a number of columns equal to the number of pools. Contains soil organic nitrogen [\ifelse{html}{\out{tN ha<sup>-1</sup>}}{\eqn{\mathrm{tN \, ha^{-1}}}}]. Only generated when \code{calcN = TRUE}.}
  \item{Nloss}{matrix with a number of rows corresponding to \code{t_sim} and a number of columns equal to the number of pools. Contains nitrogen losses [\ifelse{html}{\out{tN ha<sup>-1</sup>}}{\eqn{\mathrm{tN \, ha^{-1}}}}]. 
  Positive values indicate that nitrogen was lost in the pools between this and the previous time steps (taking nitrogen decomposition and input into account). Only generated when \code{calcN = TRUE}.}
  \item{Nmin}{matrix with a number of rows corresponding to \code{t_sim} and a number of columns equal to the number of pools. Contains nitrogen mineralisation [\ifelse{html}{\out{tN ha<sup>-1</sup>}}{\eqn{\mathrm{tN \, ha^{-1}}}}].  If values are negative, nitrogen immobilisation exceeds mineralisation. Only generated when \code{calcN = TRUE}.}
  \item{Nmin.sink.1, ..., Nmin.sink.n}{matrices with a number of rows corresponding to \code{t_sim} and a number of columns equal to the number of pools \code{n}. Contain pool-specific nitrogen mineralisation sinks [\ifelse{html}{\out{tN ha<sup>-1</sup>}}{\eqn{\mathrm{tN \, ha^{-1}}}}] (from the pool according to variable index [1, ..., n] to the pool according to column number). If the sink is the pool itself (index equals column number) the amount of decomposition is recorded. Only generated when \code{calcN = TRUE}.}
  \item{Nbalance}{matrix with a number of rows corresponding to \code{t_sim} and three columns. Contains information on overall N changes in the soil between two time steps (first column) and information on total system N balance calculated based on total \code{Nloss} (second column) and based on total \code{Nmin} (third column) [\ifelse{html}{\out{tN ha<sup>-1</sup>}}{\eqn{\mathrm{tN \, ha^{-1}}}}]. Only generated when \code{calcN = TRUE} and \code{calcNbalance = TRUE}.}
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
  #1 Example of RothC application with fictional input for a single site

  #1.1 Input

  data(RothC_Cin_ex, RothC_Nin_ex, RothC_N0_ex, RothC_C0_ex, RothC_xi_ex, 
    RothC_site_ex, RothC_env_in_ex)  #fictional data
  
  #1.2 Simulations
  
  #In the following two methods are presented, one with a RothC as a predefined model (1.2.1), 
  #one where the RothC rate modifying factors must be calculated beforehand (1.2.2). 
  #Both methods lead to the same results.

  #1.2.1 Simulation with predefined model
  
  out_rothC <- sorcering( model="RothC", site=RothC_site_ex, env_in=RothC_env_in_ex, t_sim=60, 
  Cin=RothC_Cin_ex, Nin=RothC_Nin_ex, N0=RothC_N0_ex, C0=RothC_C0_ex, calcN=TRUE, tsteps="monthly")
  
  #1.2.2 Simulation with own model definition and rate modifying factor definition

  A_RothC <- fget_A_RothC(clay=30) #create transfer matrix for RothC
  out_rothC_own <- sorcering(A=A_RothC, xi=RothC_xi_ex, t_sim=60, Cin=RothC_Cin_ex, 
    Nin=RothC_Nin_ex, N0=RothC_N0_ex, C0=RothC_C0_ex, calcN=TRUE, tsteps="monthly")
  #Note that RothC_xi_ex contains site and model specific rate modifying factors that are only valid
  #in this specific example. Generally, xi must be calculated by the user for different 
  #environmental conditions and SOC models used. 

  #1.3 Results

  #output structure summary
  summary(out_rothC)
  
  #show that results of 1.2.1 and 1.2.2 differ negligibly
  all( abs(out_rothC$C-out_rothC_own$C) < 1e-14)
  all( abs(out_rothC$N-out_rothC_own$N) < 1e-14)

  #example plot
    oldpar <- par(no.readonly = TRUE) #save old par 
  par(mfrow=c(1,1),mar=c(4,4,1,4))
  plot(rowSums(out_rothC$N),axes=FALSE, col=1, cex.lab=2,xlab="",ylab="",ylim=c(0,9),pch=20)
  par(new=TRUE)
  plot(rowSums(RothC_Cin_ex)/rowSums(RothC_Nin_ex),
    axes=FALSE,col=2, cex.lab=2,xlab="",ylab="",ylim=c(0,60),pch=20)
  axis(side=2, pos = 0,
    labels = (0:6)*1.5, at=(0:6)*10, hadj=0.7, padj = 0.5, cex.axis=2,las=1,col.axis=1)
  axis(side=4, pos = 60,
    labels = (0:6)*10, at=(0:6)*10, hadj=0, padj = 0.5, cex.axis=2, las=1,col.axis=2)
  axis(side=1, pos = 0,
    labels =  (0:6)*10 , at=(0:6)*10, hadj=0.5, padj = 0, cex.axis=2)
  title(ylab=expression("total N  [t ha"^-1*"]"), line=2, cex.lab=2)
  mtext("C input / N input", side=4, line=2, cex=2,col=2)
  title(xlab="time", line= 2, cex.lab=2)    
  par(oldpar) #back to old par
  
  #2 Example of RothC application with fictional input for a multiple site application
  
  #2.1 Input

  #fictional data
  data(RothC_Cin_ex_sl, RothC_Nin_ex_sl, RothC_N0_ex, RothC_C0_ex, 
    RothC_site_ex, RothC_env_in_ex) 
  
  #2.2. Simulation
  
  out_multi_rothC <- sorcering( model="RothC", site_sl=rep(list(RothC_site_ex),3), 
    env_in_sl=rep(list(RothC_env_in_ex),3), t_sim_sl=list(60,60,60), 
    Cin_sl=RothC_Cin_ex_sl, Nin_sl=RothC_Nin_ex_sl, N0_sl=rep(list(RothC_N0_ex),3), 
    C0_sl=rep(list(RothC_C0_ex),3), calcN=TRUE, tsteps="monthly", multisite=TRUE, 
    sitelist=list("normal","half_input","double_Cin"))

  #2.3 Results

  #output structure summary
  summary(out_multi_rothC$normal)
  summary(out_multi_rothC$half_input)
  summary(out_multi_rothC$double_Cin)
  
  #example plot
  oldpar <- par(no.readonly = TRUE) #save old par 
  par(mfrow=c(1,1),mar=c(4,4,1,4))
  for (listelement in c(1:3))
  {
    lwidth<-1
    if (listelement==2)lwidth<-3
    plot(rowSums(out_multi_rothC[[listelement]]$N),axes=FALSE, col=1,type="l", lwd=lwidth,
        lty=listelement+2,cex.lab=2,xlab="",ylab="",ylim=c(0,18))
    par(new=TRUE)
    plot(rowSums(RothC_Cin_ex_sl[[listelement]])/rowSums(RothC_Nin_ex_sl[[listelement]]),
        type="l", lwd=lwidth, lty=listelement+2,axes=FALSE,col=2, cex.lab=2,xlab="",
        ylab="",ylim=c(0,120))
    par(new=TRUE)
  }
  axis(side=2, pos = 0,
    labels = (0:6)*3, at=(0:6)*20, hadj=0.7, padj = 0.5, cex.axis=2,las=1,col.axis=1)
  axis(side=4, pos = 60,
    labels = (0:6)*20, at=(0:6)*20, hadj=0, padj = 0.5, cex.axis=2, las=1,col.axis=2)
  axis(side=1, pos = 0,
    labels =  (0:6)*10 , at=(0:6)*10, hadj=0.5, padj = 0, cex.axis=2)
  title(ylab=expression("total N  [t ha"^-1*"]"), line=2, cex.lab=2)
  mtext("C input / N input", side=4, line=2, cex=2,col=2)
  title(xlab="time", line= 2, cex.lab=2)    
  legend(x=40,y=100,legend = c("normal","half_input","double_Cin"),lty = c(3,4,5),lwd=c(1,3,1))
  par(oldpar) #back to old par
  
  #3 Example of RothC application with fictional input 
  #and fictional measurement data to calculate C0 and N0
  
  #3.1 Input

  #fictional data  
  data(RothC_Cin_ex_sl, RothC_Nin_ex_sl, RothC_site_ex, RothC_env_in_ex, meas_data_ex) 

  #3.2. Simulation
  
  out_rothC_C0<-sorcering( model="RothC", site=RothC_site_ex, env_in=RothC_env_in_ex, t_sim=60, 
    Cin=RothC_Cin_ex, Nin=RothC_Nin_ex, calcC0=TRUE, calcN=TRUE, calcN0=TRUE, tsteps="monthly", 
    meas_data=meas_data_ex) 

  #3.3 Results

  #output structure summary
  summary(out_rothC_C0)

  #example plot 
  oldpar <- par(no.readonly = TRUE) #save old par 
  par(mfrow=c(1,1),mar=c(4,4,1,4))
  plot(rowSums(out_rothC_C0$N),axes=FALSE, col=1, cex.lab=2,xlab="",ylab="",ylim=c(0,9),
    type="l",lwd=1)
  par(new=TRUE)
  plot(rowSums(out_rothC_C0$C),axes=FALSE, col=2, cex.lab=2,xlab="",ylab="",ylim=c(0,90),
    type="l",lwd=1)
  par(new=TRUE)
  plot(x=meas_data_ex[,1],y=meas_data_ex[,3],axes=FALSE, col=1, cex.lab=2,xlab="",ylab="",
    xlim=c(0,length(rowSums(out_rothC_C0$N))),ylim=c(0,9),pch=4,cex=3)
  par(new=TRUE)
  plot(x=meas_data_ex[,1],y=meas_data_ex[,2],axes=FALSE, col=2, cex.lab=2,xlab="",ylab="",
    xlim=c(0,length(rowSums(out_rothC_C0$N))),ylim=c(0,90),pch=4,cex=3)
  par(new=TRUE)
  axis(side=2, pos = 0,            
    labels = (0:8)*1, at=(0:8)*10, hadj=1, padj = 0.5, cex.axis=2,las=1,col.axis=1)
  axis(side=4, pos = 60,
    labels = (0:8)*10, at=(0:8)*10, hadj=0, padj = 0.5, cex.axis=2, las=1,col.axis=2)
  axis(side=1, pos = 0,
    labels =  (0:8)*10 , at=(0:8)*10, hadj=0.5, padj = 0, cex.axis=2)
  title(ylab=expression("SON [t ha"^-1*"]"), line=2, cex.lab=2)
  mtext(expression("SOC [t ha"^-1*"]"), side=4, line=3, cex=2,col=2)
  title(xlab="time", line= 2, cex.lab=2)  
  legend(x=30,y=30,legend = c("model result","measurement"),lwd=c(1,0))
  legend(x=31,y=30,legend = c("",""),pch=4,pt.cex=c(0,3),bty="n")
  par(oldpar) #back to old par
  
  #4 Example of Yasso15 application using multiple sites and 
  #input values of different wood diameters which take uncertainties into account
  
  #4.1 Input

  data(Yasso_Cin_ex_wood_u_sl, Yasso_Nin_ex_wood_u_sl, Yasso_C0_ex_sl, 
    Yasso_N0_ex_sl, RothC_env_in_ex) #fictional data 
  
  #show last entries of C input for 3rd site, 2nd wood layer, 4th uncertainty layer
  tail(Yasso_Cin_ex_wood_u_sl[[3]][[2]][[4]])

  #diameter of wood input: 2 classes of 0 cm and 10 cm for each of the 3 sites 
  wood_diam_ex_sl<-list(c(0,10),c(0,10),c(0,10)) 
    
  #environmental variables
  Yasso_env_in_ex<-RothC_env_in_ex[,1:2]
    
  #4.2 Simulation
  
  out_multi_yasso_wood_unc <- sorcering( model="Yasso15", env_in_sl=rep(list(Yasso_env_in_ex),3), 
    t_sim_sl=list(60,60,60),wood_diam_sl=wood_diam_ex_sl, tsteps="monthly", multisite=TRUE,
    Cin_wood_sl=Yasso_Cin_ex_wood_u_sl, Nin_wood_sl=Yasso_Nin_ex_wood_u_sl, 
    N0_sl=Yasso_N0_ex_sl, C0_sl=Yasso_C0_ex_sl, calcN=TRUE, sitelist=list("a","b","c"))
    
  #4.3 Results
  
  #show the last C results for 3rd site, 4th uncertainty layer
  tail(out_multi_yasso_wood_unc[[3]][[4]]$C) 
  
  #5 Example of RothC application using stochastically varying parameters and multiple sites 

  #5.1 Fictional data 
  data(RothC_Cin_ex_sl, RothC_Nin_ex_sl, RothC_C0_ex, RothC_N0_ex, 
  RothC_site_ex, RothC_env_in_ex)
  
  #standard deviations [%] used for each of the 7 RothC theta parameters 
  RothC_theta_unc <- c(0,0,1,1,1,1,2)
  
  #5.2 Simulation
  
  out_sl <- sorcering( model="RothC", site_sl=rep(list(RothC_site_ex),3), 
  env_in_sl=rep(list(RothC_env_in_ex),3), t_sim_sl=list(60,60,60),
  Cin_sl=RothC_Cin_ex_sl, Nin_sl=RothC_Nin_ex_sl, C0_sl=rep(list(RothC_C0_ex),3), 
  N0_sl=rep(list(RothC_N0_ex),3),calcN=TRUE,theta_n_unc=10,
  theta_unc=RothC_theta_unc, multisite=TRUE, 
  sitelist=list("normal","half_input","double_Cin"))

  #5.3 Means and standard deviation
  
  #60 time steps, 5 pools, 9 output types, 10 theta_n_unc, 3 sites
  out_sl_arr <- array(unlist(out_sl),c(60,5,9,10,3)) 
  out_sl_arr_N <- out_sl_arr[,,2,,] #only output type 2: N
  #mean over all uncerts
  out_sl_arr_N_mean <- apply( out_sl_arr_N , c(1,2,4), na.rm=TRUE, FUN=mean ) 

  #standard deviation
  out_sl_arr_N_sd<-
  array(0, dim=c(dim(out_sl_arr_N)[1],dim(out_sl_arr_N)[2],dim(out_sl_arr_N)[4]))
  for (dim3 in c(1:dim(out_sl_arr_N)[4])) 
  out_sl_arr_N_sd[,,dim3]<-apply(out_sl_arr_N[,,,dim3],c(1:2),sd)

  #5.4 Results
  
  #show the last N means for stand 1 
  tail(out_sl_arr_N_mean[,,1])

  #show the last N standard deviations for stand 1 
  tail(out_sl_arr_N_sd[,,1])  
  
  #6 Example of how to create input lists for a RothC application using stochastically 
  #varying inputs and input scenarios

  #6.1 Input
  
  #fictional data 
  data(RothC_Cin_ex_sl, RothC_C0_ex, RothC_site_ex, RothC_env_in_ex)  

  #create input list of 3 scenarios, 100 uncertainties each
  set.seed(17) #to make 'random' results reproducible
  f1<-1
  for (no in c(1:3)) #loop over 3 input scenarios
  {
      #normal, half and double input
      Cin <- switch (no, RothC_Cin_ex, RothC_Cin_ex/2, RothC_Cin_ex*2)
      f2 <- 1
      #create fictional uncertainties
      for (unc in c(1:100)) #loop over 100 uncertainties
      {
          randnum<-max(0,rnorm(1,1,0.5)) #out of normal dist. with 50% sd.
          if (f2==1) Cin_u <- list(Cin*randnum) else
          Cin_u[[length(Cin_u)+1]] <- Cin*randnum
          f2 <- 0
      }
      if (f1==1) Cin_u_sl <- list(Cin_u) else
      Cin_u_sl[[length(Cin_u_sl)+1]] <- Cin_u
      f1 <- 0
  }

  #show input of scenario 3, uncertainty 51
  head(Cin_u_sl[[3]][[51]])

  #6.2 Simulation
  out_sl <- sorcering( model="RothC", site_sl=rep(list(RothC_site_ex),3), 
  env_in_sl=rep(list(RothC_env_in_ex),3), t_sim_sl=list(60,60,60),
  Cin_sl=Cin_u_sl, C0_sl=list(RothC_C0_ex,RothC_C0_ex,RothC_C0_ex), tsteps="monthly", 
  multisite=TRUE, sitelist=list("normal","half_input","double_Cin"))
  
  #6.3 Means and standard deviation
  
  #60 time steps, 5 pools, 1000 uncertainties, 3 sites
  out_sl_arr <- array(unlist(out_sl),c(60,5,100,3)) 
  
  #means
  out_sl_arr_mean <- apply( out_sl_arr , c(1,2,4), na.rm=TRUE, FUN=mean ) 

  #standard deviation
  out_sl_arr_sd<-
  array(0, dim=c(dim(out_sl_arr)[1],dim(out_sl_arr)[2],dim(out_sl_arr)[4]))
  for (dim3 in c(1:dim(out_sl_arr)[4])) 
  out_sl_arr_sd[,,dim3]<-apply(out_sl_arr[,,,dim3],c(1:2),sd)
  
  #6.4 Results
  
  #C-pool sums of means for the 3 scenarios
  totalC_m1<-rowSums(out_sl_arr_mean[,,1])
  totalC_m2<-rowSums(out_sl_arr_mean[,,2])
  totalC_m3<-rowSums(out_sl_arr_mean[,,3])
  
  #C-pool sums of standard deviations for the 3 scenarios
  totalC_s1<-rowSums(out_sl_arr_sd[,,1])
  totalC_s2<-rowSums(out_sl_arr_sd[,,2])
  totalC_s3<-rowSums(out_sl_arr_sd[,,3])

  #example plot
  oldpar <- par(no.readonly = TRUE) #save old par 
  par(mfrow=c(1,1),mar=c(4,4,1,4))
  plot(totalC_m1,axes=FALSE, col=2, cex.lab=2,xlab="",ylab="",ylim=c(0,100),
      type="l",lwd=1)
  par(new=TRUE)
  plot(totalC_m2,axes=FALSE, col=3, cex.lab=2,xlab="",ylab="",ylim=c(0,100),
      type="l",lwd=1)
  par(new=TRUE)
  plot(totalC_m3,axes=FALSE, col=4, cex.lab=2,xlab="",ylab="",ylim=c(0,100),
      type="l",lwd=1)
  par(new=TRUE)
  polygon(c(1:60,60:1),c(totalC_m1+totalC_s1, rev(totalC_m1-totalC_s1)),
  border=NA,col=rgb(1,0,0,0.27),density=40,angle=180,xlab="",ylab="") 
  par(new=TRUE)
  polygon(c(1:60,60:1),c(totalC_m2+totalC_s2, rev(totalC_m2-totalC_s2)),
  border=NA,col=rgb(0,1,0,0.27),density=30,xlab="",ylab="") 
  par(new=TRUE)
  polygon(c(1:60,60:1),c(totalC_m3+totalC_s3, rev(totalC_m3-totalC_s3)),
  border=NA,col=rgb(0,0,1,0.27),density=25,angle=90,xlab="",ylab="") 
  par(new=TRUE)
  axis(side=2, pos = 0,
      labels = (0:10)*1, at=(0:10)*10, hadj=1, padj = 0.5, cex.axis=2,las=1,col.axis=1)
  axis(side=1, pos = 0,
      labels =  (0:6)*10 , at=(0:6)*10, hadj=0.5, padj = 0, cex.axis=2)
  title(ylab=expression("SOC [t ha"^-1*"]"), line=2, cex.lab=2)
  title(xlab="time", line= 2, cex.lab=2)  
  legend(x=20,y=30,fill=c(0,0,0,4,2,3),density=c(0,0,0,25,40,30),angle=c(0,0,0,90,0,45),
  border=c(0,0,0,1,1,1),legend = c("mean double input scenario","mean regular input scenario",
  "mean half input scneario","uncertainty range double input scenario",
  "uncertainty range regular input scenario","uncertainty range half input scenario"))
  legend(x=20,y=30,lty=c(1,1,1,0,0,0),seg.len=c(1,1,1,0,0,0), col=c(4,2,3,0,0,0),
  legend = c("","","","","",""),bty="n")
  par(oldpar) #back to old par
  
}
