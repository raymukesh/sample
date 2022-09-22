function (x, aggregate = 1)
{
    resampleFactor <- aggregate
    inputRaster <- x
    inCols <- ncol(inputRaster)
    inRows <- nrow(inputRaster)
    resampledRaster <- rast(ncol = (inCols/resampleFactor), nrow = (inRows/resampleFactor),
        crs = crs(inputRaster))
    ext(resampledRaster) <- ext(inputRaster)
    y <- resample(inputRaster, resampledRaster, method = "near")
    coords <- xyFromCell(y, seq_len(ncell(y)))
    dat <- stack(values(y, dataframe = TRUE))
    names(dat) <- c("value", "variable")
    dat$value[is.nan(dat$value)] <- NA
    dat <- cbind(coords, dat)
    dat
}