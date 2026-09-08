#################################################################################################### .
#   Description: Load and call the implementation bundled with a common CyneRgy example.
#################################################################################################### .

.CommonExampleFunctionCache <- new.env( parent = emptyenv() )


.GetCommonExampleFunction <- function( strExample, strFile, strFunction )
{
    strKey <- paste( strExample, strFile, strFunction, sep = "/" )

    if( !exists( strKey, envir = .CommonExampleFunctionCache, inherits = FALSE ) )
    {
        strPath <- system.file( "Examples", strExample, "R", strFile, package = "CyneRgy" )
        if( !nzchar( strPath ) || !file.exists( strPath ) )
            stop( "The bundled implementation for ", strFunction, " could not be found.", call. = FALSE )

        envFunction <- new.env( parent = asNamespace( "CyneRgy" ) )
        envFunction$library <- function( package, ... )
        {
            strPackage <- as.character( substitute( package ) )
            if( identical( strPackage, "CyneRgy" ) )
                return( invisible( TRUE ) )

            loadNamespace( strPackage )
            for( strExport in getNamespaceExports( strPackage ) )
                assign( strExport, getExportedValue( strPackage, strExport ), envir = envFunction )

            return( invisible( TRUE ) )
        }

        sys.source( strPath, envir = envFunction, chdir = TRUE )
        if( !exists( strFunction, envir = envFunction, inherits = FALSE ) ||
            !is.function( get( strFunction, envir = envFunction, inherits = FALSE ) ) )
            stop( "The bundled example does not define ", strFunction, ".", call. = FALSE )

        assign( strKey, get( strFunction, envir = envFunction, inherits = FALSE ),
                envir = .CommonExampleFunctionCache )
    }

    return( get( strKey, envir = .CommonExampleFunctionCache, inherits = FALSE ) )
}


.CallCommonExampleFunction <- function( strExample, strFile, strFunction, lArguments )
{
    fnExample <- .GetCommonExampleFunction( strExample, strFile, strFunction )
    return( do.call( fnExample, lArguments ) )
}
