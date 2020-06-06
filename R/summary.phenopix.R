summary.phenopix <- function(object, ...) {
    cat('\nData\n')
    observed <- object$data
    print(summary(observed))
    cat('\nPredicted\n')
    predicted <- object$fit$fit$predicted
    print(summary(predicted))
    cat('\nFormula\n')
    print(object$fit$fit$formula)
    cat('\nThresholds\n')
    print(object$metrics)
}