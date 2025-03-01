function (vec, lower = NULL, upper = NULL) 
{
    stopifnot(is.numeric(vec))
    stopifnot(length(vec) > 0)
    if (is.null(lower) | is.null(upper)) {
        boundaries <- plus_minus_SD(vec, 1)
        lower <- boundaries["lower"]
        upper <- boundaries["upper"]
    }
    vec <- ifelse(vec < lower | vec > upper, NA, vec)
    return(vec)
}
