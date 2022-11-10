TFM <- function(n, schedule, t.schedule, tfm.mortality, pops) {
  larvae    <- pops[["larvae"]] # larvae that are alread alive;

  s.year  <- t.schedule[n-(first.year-1)]  # what treatment schedule year are we using
  streams <- schedule[[s.year]]

  m1 <- match(larvae[, 1], streams)
  m2 <- which(is.na(m1) == FALSE)
  untreated <- larvae[-(m2), ]  # isolate streams with larva to not be treated this year

  #keep resitant indivudals alive
  treated   <- larvae[m2, ] # isolate streams to treat with TFM
  resistant <- treated[treated[, 5] == 1, ]
  treated   <- treated[treated[, 5] != 1, ] # remove resiant individuals from treatment (dont want to add them twice by accident)

  n.larva   <- nrow(treated)
  n.kill    <- floor(n.larva * tfm.mortality) # could add variance in tfm.mortality - none added here yet
  keep      <- sample(1:n.larva, n.larva-n.kill, replace = FALSE) # randomly sample indivduals who survive treatement
  survivors <- treated[keep, ]

  larvae <- rbind(untreated, survivors, resistant)
  #larvae <- larvae[sort(larvae[, 1])]

  pops[["larvae"]] <- larvae
  return(pops)
}



