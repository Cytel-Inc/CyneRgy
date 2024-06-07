#'@name GenerateCensoringUsingBinomialProportion
#'@author Shubham Lahoti, J. Kyle Wathen
#'@description  Generate censoring indicator ( CensorInd ) for 2 arm designs with Normal and Binomial Endpoint using a single dropout probability.  
#'               
#'@param NumSub The integer value specifying the number of patients or subjects in the trial. The numeric value of the argument value is sent in when called.
#'@param ProbDrop A Dropout probability for both the arms. The numeric value is sent to the .
#'@param UserParam A list of user defined parameters in East. The default must be NULL. It is an optional parameter.  
#' 
#' @return A list that contains: 
#' \describe{
#'     \item{ErrorCode (Optional)}{An integer value:  ErrorCode = 0 --> No Error
#'                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#'                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.}
#'     \item{CensorInd (Mandatory)}{A vector of length NumSub of censor indicator values with 0 for patients that dropout eg non-completer, 1 for no dropout, eg compelter. }
GenerateCensoringUsingBinomialProportion <- function( NumSub, ProbDrop,  UserParam = NULL ) 
{   

    Error 	            <- 0
                         
    vCensoringIndicator <- rbinom( n = NumSub, size = 1, prob = 1 - ProbDrop )
 
    return( list( CensorInd = as.integer( vCensoringIndicator ), ErrorCode = as.integer( Error ) ) );
}



