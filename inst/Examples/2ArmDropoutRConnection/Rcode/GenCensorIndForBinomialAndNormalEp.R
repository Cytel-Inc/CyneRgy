#'@name GenCensorInd
#'@Endpoints : 2 arm Designs (Binomial, Continuous)
#'@author Shubham Lahoti
#'@description : The following function generates Censor ID for Normal and Binomial Endpoint.
#'@param NumSub: The number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#'@param ProbDrop: A Dropout probability for both the arms. The argument value is passed from Engine.
#'@param UserParam A list of user defined parameters in East. The default must be NULL. It is an optional parameter.


#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.

#' @return retval : This is a binary vector where 1 means completer and 0 means non-completer
#' 

# Example -1 : Generate CensorInd for Binomial and Normal Design

GenCensorIndForBinomialAndNormal <- function( NumSub, ProbDrop,  UserParam = NULL ) 
{   

    Error 	           <- 0
                         
    retval             <- rbinom( n = NumSub, size = 1, prob = 1 - ProbDrop )
 
    return( list( CensorInd = as.integer( retval ), ErrorCode = as.integer( Error ) ) );
}



