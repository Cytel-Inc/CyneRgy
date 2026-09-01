#################################################################################################### .
#   Description: Common randomization functions.
#################################################################################################### .
#' @name RandomizationSubjectsUsingUniformDistribution
#' @title Randomize Subjects Between Two Arms
#'
#' @description Calls the implementation from the common `RandomizeSubjects` example. Randomly assigns subjects to control
#' (`0`) and experimental (`1`) arms while enforcing the requested final allocation counts.
#'
#' @param NumSub Integer number of subjects to randomize.
#' @param NumArms Integer number of trial arms. This function supports two arms.
#' @param AllocRatio Numeric experimental-to-control allocation ratio.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the randomization integration point.
#' @export
#################################################################################################### .

RandomizationSubjectsUsingUniformDistribution <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "RandomizeSubjects", "RandomizationSubjectsUsingUniformDistribution.R", "RandomizationSubjectsUsingUniformDistribution",
        list( NumSub = NumSub, NumArms = NumArms, AllocRatio = AllocRatio, UserParam = UserParam )
    ) )
}
