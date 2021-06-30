.quadrances <- function ( x , table ) {
    DD <- t ( table ) - x
    DD <- DD * DD
    vapply (
        USE.NAMES = FALSE ,
        FUN.VALUE = vector (
            mode = typeof ( DD ) ,
            length = 1L ) ,
        X = seq_len ( ncol ( DD ) ) ,
        FUN = function ( j ) sum ( DD [ , j ] ) ) }

quadrances <- function ( x , table ) vapply (
    USE.NAMES = FALSE ,
    X = seq_len ( nrow ( x ) ) ,
    FUN = function ( i ) .quadrances ( x [ i , , drop = TRUE ] , table ) ,
    FUN.VALUE = .quadrances ( x [ 1 , , drop = TRUE ] , table ) )
