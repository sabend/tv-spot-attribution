source("src/BaselineModel/BaselineModel.R")

FourierModel <- R6Class(
  "FourierModel",
   # INHERIT
   inherit = BaselineModel,
   
   # PRIVATE
   private = list(),
   
   # PUBLIC
   public = list(
     # FUN: initialize @overwrite
     initialize = function(params, traffic, ad_locations) {
       super$initialize(params, traffic, ad_locations)
       
       stopifnot(all(FourierModel$param_names() %in% names(params)))
     },
     
     # FUN: calculate @overwrite
     calculate = function(ad_start_index, baseline_window) {
       super$calculate(ad_start_index, baseline_window)
       traffic <- private$.traffic
       
       # PROCESSING
       # vars
       x <- traffic
       n <- length(x)
       cutoff_frequency <- as.numeric(private$.params$CUTOFF_FREQUENCY)
       stopifnot(cutoff_frequency <= 0.5)
       # Construct the filter
       filter_lowpass <- rep(1, n)
       filter_lowpass[round(cutoff_frequency * n):(n-round(cutoff_frequency * n + 2))] <- 0
       # Go in the spectral domaine
       x_hat <- fft(x)
       x_hat_filtered <- x_hat * filter_lowpass # Filter the index
       # Go back in the temporal domaine
       x_filtered <- fft(x_hat_filtered, inverse = TRUE)/n
       
       
       baseline <- abs(x_filtered[ad_start_index:(ad_start_index + baseline_window-1)])
       
       return(baseline)
     }
   )
)

# STATIC
FourierModel$.param_names = c("CUTOFF_FREQUENCY")
FourierModel$param_names = function() {
  return(FourierModel$.param_names)
}