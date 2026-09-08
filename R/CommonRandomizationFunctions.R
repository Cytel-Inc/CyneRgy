#################################################################################################### .
#   Description: Common randomization functions for two-arm and multi-arm clinical trial designs.
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

RandomizationSubjectsUsingUniformDistribution <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "RandomizeSubjects", "RandomizationSubjectsUsingUniformDistribution.R", "RandomizationSubjectsUsingUniformDistribution",
        list( NumSub = NumSub, NumArms = NumArms, AllocRatio = AllocRatio, UserParam = UserParam )
    ) )
}


#' @name RandomizationSubjectsUsingSampleFunctionInR
#' @title Randomize Subjects to Two Arms Using R's `sample()` Function
#'
#' @description Calls the `sample()` implementation from the common `RandomizeSubjects` example.
#'
#' @param NumSub Integer number of subjects to randomize.
#' @param NumArms Integer number of trial arms. This function supports two arms.
#' @param AllocRatio Numeric experimental-to-control allocation ratio.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the randomization integration point.
#' @export

RandomizationSubjectsUsingSampleFunctionInR <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "RandomizeSubjects", "RandomizationSubjectsUsingSampleFunctionInR.R", "RandomizationSubjectsUsingSampleFunctionInR",
        list( NumSub = NumSub, NumArms = NumArms, AllocRatio = AllocRatio, UserParam = UserParam )
    ) )
}


#' @name BlockRandomizationSubjectsUsingRPackage
#' @title Permuted Block Randomization for Two-Armed Trials
#'
#' @description Calls the permuted-block implementation from the common `RandomizeSubjects` example. This function requires
#' the suggested `randomizeR` package.
#'
#' @param NumSub Integer number of subjects to randomize.
#' @param NumArms Integer number of trial arms. This function supports two arms.
#' @param AllocRatio Numeric experimental-to-control allocation ratio.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the randomization integration point.
#' @export

BlockRandomizationSubjectsUsingRPackage <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "RandomizeSubjects", "BlockRandomizationSubjectsUsingRPackage.R", "BlockRandomizationSubjectsUsingRPackage",
        list( NumSub = NumSub, NumArms = NumArms, AllocRatio = AllocRatio, UserParam = UserParam )
    ) )
}


#' @name RandomizeSubjectsAcrossMultipleArms
#' @title Randomize Subjects Across Multiple Arms
#'
#' @description Calls the multiple-arm implementation from the common `RandomizeSubjects` example.
#'
#' @param NumSub Integer number of subjects to randomize.
#' @param NumArms Integer number of trial arms.
#' @param AllocRatio Numeric allocation ratios for the experimental arms relative to the control arm; its length is `NumArms - 1`.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the randomization integration point.
#' @export

RandomizeSubjectsAcrossMultipleArms <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "RandomizeSubjects", "RandomizeSubjectsAcrossMultipleArms.R", "RandomizeSubjectsAcrossMultipleArms",
        list( NumSub = NumSub, NumArms = NumArms, AllocRatio = AllocRatio, UserParam = UserParam )
    ) )
}
