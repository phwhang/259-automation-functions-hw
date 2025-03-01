function (vec, num_of_SDs = 1) 
{
    vec_mean <- mean(vec)
    vec_sd <- sd(vec)
    boundary <- num_of_SDs * vec_sd
    lower <- vec_mean - boundary
    upper <- vec_mean + boundary
    return(c(lower = lower, upper = upper))
}
