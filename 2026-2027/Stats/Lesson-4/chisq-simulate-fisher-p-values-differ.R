## Are p-values from fisher.test and chisq.test with simulate.p.value
## the same? Answer is not always.

### Are p-values the same; a boring, overkill, simulation. Probably skip to "Simulate until we get something interesting"

simchif <- function(nr = 2, nc = 3, B = 3e6, N = 100, outm = FALSE) {
    while (TRUE) {
        m <- matrix(rmultinom(1, size = 100, prob = runif(nr * nc)),
                    nrow = nr)
        if (isTRUE(all(colSums(m) > 0)) &&
            isTRUE(all(rowSums(m) > 0))) break
    }
    fp <- try(fisher.test(m)$p.value, silent = TRUE)
    fps <- fisher.test(m, simulate.p.value = TRUE, B = B)$p.value
    fps2 <- fisher.test(m, simulate.p.value = TRUE, B = B)$p.value
    chs <- chisq.test(m, simulate.p.value = TRUE, B = B)$p.value
    chs2 <- chisq.test(m, simulate.p.value = TRUE, B = B)$p.value
    if (outm) {
        mout <- NA
        if (abs(fps - chs) > 1e5) mout <- m
    }
    return(c(fisher_p = ifelse(inherits(fp, "try-error"), NA, fp),
             fisher_sim = fps,
             chi_sim = chs,
             fisher_sim2 = fps2,
             chi_sim2 = chs2,
             m = ifelse(outm, mout, NA)
             ))
}


library(parallel)
p_2_2 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 2), mc.cores = 36))
p_2_3 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 3), mc.cores = 36))

p_2_4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 4), mc.cores = 36))
p_2_5 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 5), mc.cores = 36))
p_3_3 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 3, nc = 3), mc.cores = 36))
## Many non-simul fail
p_3_4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 3, nc = 4), mc.cores = 36))
p_4_4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 4, nc = 4), mc.cores = 36))
p_5_5 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 5, nc = 5), mc.cores = 36))


p_2_2_N1000 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 2, N = 1000),
                                       mc.cores = 36))
p_2_3_N1000 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 3, N = 1000),
                                       mc.cores = 36))
p_2_4_N1000 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 4, N = 1000),
                                       mc.cores = 36))
p_2_5_N1000 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 5, N = 1000),
                                       mc.cores = 36))
p_3_3_N1000 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 3, nc = 3, N = 1000),
                                       mc.cores = 36))

p_3_4_N1000 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 3, nc = 4, N = 1000),
                                       mc.cores = 36))
p_4_4_N1000 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 4, nc = 4, N = 1000),
                                       mc.cores = 36))
p_5_5_N1000 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 5, nc = 5, N = 1000),
                                       mc.cores = 36))


p_2_2_Nx4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 2, N = nr * nc * 4),
                                     mc.cores = 36))
p_2_3_Nx4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 3, N = nr * nc * 4),
                                     mc.cores = 36))
p_2_4_Nx4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 4, N = nr * nc * 4),
                                     mc.cores = 36))
p_2_5_Nx4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 2, nc = 5, N = nr * nc * 4),
                                     mc.cores = 36))
p_3_3_Nx4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 3, nc = 3, N = nr * nc * 4),
                                     mc.cores = 36))

p_3_4_Nx4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 3, nc = 4, N = nr * nc * 4),
                                     mc.cores = 36))
p_4_4_Nx4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 4, nc = 4, N = nr * nc * 4),
                                     mc.cores = 36))
p_5_5_Nx4 <- do.call(rbind, mclapply(1:1000, function(x) simchif(nr = 5, nc = 5, N = nr * nc * 4),
                                     mc.cores = 36))

save.image(file = "sims_chi_fisher.RData")



load("sims_chi_fisher.RData")

## se: ~ pq/sqrt(3e6)
sep <- function(p) return(sqrt(p * (1 - p) / 3e6))
sapply(c(2e-6, 1e-6, 3e-6, 5e-5, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 0.15), sep)
## But for 1e-6 enough with 3. For 1e-5, we need 30.

controlplotsim <- function(thedata, lims = c(1e-6, 0.15), log = "") {
    title <- deparse(substitute(thedata))
    plot(chi_sim ~ chi_sim2, data = thedata,
         main = paste0(title, "_Chi"), xlim = lims, ylim = lims, log = log)
    abline(a = 0, b = 1)
    plot(fisher_sim ~ fisher_sim2, data = thedata,
         main = paste0(title, "_Fisher"), xlim = lims, ylim = lims, log = log)
    abline(a = 0, b = 1)
}


plotsim <- function(thedata, lims = c(1e-5, 0.15), log = "xy", B = 3e6) {
    title <- deparse(substitute(thedata))
    ## We can get even more precision
    ## This will be silly in the 2x2 as those are not by permutation
    if (title %in% c("p_2_2", "p_2_2_N1000", "p_2_2_Nx4")) {
        fs <- thedata[, "fisher_sim"]
    } else {
        fs <- (thedata[, "fisher_sim"] + thedata[, "fisher_sim2"] - (1 / (B + 1))) * ((B + 1) / (2 * B + 1))
    }
    cs <- (thedata[, "chi_sim"] + thedata[, "chi_sim2"] - (1 / (B + 1))) * ((B + 1) / (2 * B + 1))

    plot(fs ~ cs, data = thedata,
         xlab = "Fisher", ylab = "Chi",
         main = title, xlim = lims, ylim = lims, log = log)
    abline(a = 0, b = 1)
}


## Control plots

par(mfrow = c(2, 8))
controlplotsim(p_2_2, log = "xy")
controlplotsim(p_2_3, log = "xy")
controlplotsim(p_2_4, log = "xy")
controlplotsim(p_2_5, log = "xy")
controlplotsim(p_3_3, log = "xy")
controlplotsim(p_3_4, log = "xy")
controlplotsim(p_4_4, log = "xy")
controlplotsim(p_5_5, log = "xy")

par(mfrow = c(2, 8))
controlplotsim(p_2_2_N1000, log = "xy")
controlplotsim(p_2_3_N1000, log = "xy")
controlplotsim(p_2_4_N1000, log = "xy")
controlplotsim(p_2_5_N1000, log = "xy")
controlplotsim(p_3_3_N1000, log = "xy")
controlplotsim(p_3_4_N1000, log = "xy")
controlplotsim(p_4_4_N1000, log = "xy")
controlplotsim(p_5_5_N1000, log = "xy")

par(mfrow = c(2, 8))
controlplotsim(p_2_2_Nx4, log = "xy")
controlplotsim(p_2_3_Nx4, log = "xy")
controlplotsim(p_2_4_Nx4, log = "xy")
controlplotsim(p_2_5_Nx4, log = "xy")
controlplotsim(p_3_3_Nx4, log = "xy")
controlplotsim(p_3_4_Nx4, log = "xy")
controlplotsim(p_4_4_Nx4, log = "xy")
controlplotsim(p_5_5_Nx4, log = "xy")



## The plots of the output. And yes, differences in the 2x2 case too.
par(mfrow = c(2, 4))
plotsim(p_2_2)
plotsim(p_2_3)
plotsim(p_2_4)
plotsim(p_2_5)
plotsim(p_3_3)
plotsim(p_3_4)
plotsim(p_4_4)
plotsim(p_5_5)

par(mfrow = c(2, 4))
plotsim(p_2_2_N1000)
plotsim(p_2_3_N1000)
plotsim(p_2_4_N1000)
plotsim(p_2_5_N1000)
plotsim(p_3_3_N1000)
plotsim(p_3_4_N1000)
plotsim(p_4_4_N1000)
plotsim(p_5_5_N1000)

par(mfrow = c(2, 4))
plotsim(p_2_2_Nx4)
plotsim(p_2_3_Nx4)
plotsim(p_2_4_Nx4)
plotsim(p_2_5_Nx4)
plotsim(p_3_3_Nx4)
plotsim(p_3_4_Nx4)
plotsim(p_4_4_Nx4)
plotsim(p_5_5_Nx4)


## Look at simplest case
dp <- abs(p_2_2[, 1] - p_2_2[, 3])
rp <- p_2_2[, 1] / p_2_2[, 3]
p_2_2b <- cbind(p_2_2, dp, rp)
## catch some
p_2_2b[(p_2_2b[, 1] < 0.5) & (p_2_2b[, 7] > 2), ]
p_2_2b[(p_2_2b[, 1] < 0.5) & (p_2_2b[, 7] > 1.5), ]

### Simulate until we get something interesting


## Generate data and get p-values. The main workhorse.
ft_ct_seed <- function(seed,
                       nr = 2, nc = 2, N = 100,
                       B = 1e7,
                       printres = TRUE) {
    set.seed(seed)
    while (TRUE) {
        m <- matrix(rmultinom(1, size = N, prob = runif(nr * nc)),
                    nrow = nr)
        if (isTRUE(all(colSums(m) > 0)) &&
            isTRUE(all(rowSums(m) > 0))) break
    }

    ft <- try(fisher.test(m), silent = TRUE)
    fp <- ft$p.value
    ch <- chisq.test(m, simulate.p.value = TRUE, B = B)
    chp <- ch$p.value
    if (printres) {
        cat("\n ======= \n")
        print(m)
        cat("\n ======= \n")
        print(ft)
        cat("\n ======= \n")
        print(ch)
        cat("\n ======= \n")
        cat("ratio p_values = ", fp / chp, "; ", chp / fp, "\n")
    }
    return(list(m = m,
                fisher_out = ft,
                chisq_out = ch,
                fisher_p = fp,
                chisq_p = chp,
                ratio_f_c = fp/chp,
                ratio_c_f = chp/fp
                ))
}


## Loop until interesting stuff.
simchif2 <- function(nr = 2, nc = 2, seed = 100,
                     B = 1e7, N = 100,
                     min_ratio_ps = 1.8,
                     min_f_p = 1e-5,
                     max_f_p = 0.5) {
    while (TRUE) {
        set.seed(seed)
        cat("\n seed = ", seed)
        out <- ft_ct_seed(seed = seed, nr = nr, nc = nc,
                          B = B, N = N, printres = FALSE)
        seed <- seed + 1
        fp <- out$fisher_p
        chp <- out$chisq_p
        if ((fp < max_f_p) &&
            (fp > min_f_p) &&
            (((fp / chp) > min_ratio_ps) || ((chp / fp) > min_ratio_ps)))
            break
    }
    cat("\n ======= \n")
    print(out$m)
    cat("\n ======= \n")
    print(out$fisher_out)
    cat("\n ======= \n")
    print(out$chisq_out)
    cat("\n ======= \n")
    cat("ratio p_values = ", fp / chp, "; ", chp / fp, "\n")
    return(list(m = out$m,
                fisher_out = out$fisher_out,
                chisq_out = out$chisq_out,
                ratio_f_c = fp/chp,
                ratio_c_f = chp/fp
                ))
}




## Examples of runs searching
simchif2(seed = 29005)
simchif2(seed = 110, N = 15)
simchif2(seed = 200, N = 25, nr = 2, nc = 3)

## Some located examples
ft_ct_seed(109)
ft_ct_seed(279)
ft_ct_seed(809)
ft_ct_seed(34)
ft_ct_seed(1961)
ft_ct_seed(11996)
