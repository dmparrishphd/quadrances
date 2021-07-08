quadrances1to1 <- function ( xy , uv ) {
    stopifnot (
        is.matrix ( xy ) ,
        is.matrix ( uv ) ,
        is.numeric ( xy ) ,
        is.numeric ( uv ) ,
        identical ( dim ( xy ) , dim (uv ) ) )
    D <- xy - uv
    D <- D * D
    vapply (
        USE.NAMES = FALSE ,
        X = seq_len ( nrow ( D ) ) ,
        FUN.VALUE = sum ( D [ 1 , ] ) ,
        FUN = function ( i ) sum ( D [ i , ] ) ) }
