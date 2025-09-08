restore_margin_inner = function(ooma) {
  ## restore inner margin defaults
  ## (in case the plot region/margins were affected by the preceding tinyplot call)
  if (any(ooma != 0)) {
    if (ooma[1] != 0 && omar[1] == par("mgp")[1] + 1 * par("cex.lab")) {
    omar[1] = 5.1
    }
    if (ooma[2] != 0 && omar[2] == par("mgp")[1] + 1 * par("cex.lab")) {
    omar[2] = 4.1
    }
    if (ooma[3] == topmar_epsilon && omar[3] != 4.1) {
    omar[3] = 4.1
    }
    if (ooma[4] != 0 && omar[4] == 0) {
    omar[4] = 2.1
    }
    par(mar = omar)
  }
  ## restore outer margin defaults (with a catch for custom mfrow plots)
  if (all(par("mfrow") == c(1, 1))) {
    par(omd = c(0, 1, 0, 1))
  }
}
