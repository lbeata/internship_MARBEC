# Align the meridians of the different gts objects to make sure we are working with the right coordinates 

harmonize_PrimeMeridian = function(obj, target = "left", verbose = TRUE) {
  if (is.null(obj)) stop("Input object is NULL")
  
  xlon = longitude(obj)
  current_pm = if (any(xlon < 0)) "center" else "left"
  
  if (verbose) {
    message("Current meridian: ", current_pm, " | Target meridian: ", target)
  }
  
  if (current_pm != target) {
    longitude(obj) = longitude(obj, target)
    if (verbose) message("Longitude re-centered to ", target, ".")
  } else {
    if (verbose) message("No need to change longitude.")
  }
  
  return(obj)
}


align_time_period = function(sim, obs, verbose = TRUE) {
  # Get time ranges
  sim_start = start(sim)
  sim_end   = end(sim)
  obs_start = start(obs)
  obs_end   = end(obs)
  
  # Determine overlap
  overlap_start = if ((sim_start[1] > obs_start[1]) || 
                       (sim_start[1] == obs_start[1] && sim_start[2] > obs_start[2])) {
    sim_start
  } else {
    obs_start
  }
  
  overlap_end = if ((sim_end[1] < obs_end[1]) || 
                     (sim_end[1] == obs_end[1] && sim_end[2] < obs_end[2])) {
    sim_end
  } else {
    obs_end
  }
  
  if (verbose) {
    message("Aligning time period...")
    message("→ Sim: ", sim_start, " to ", sim_end)
    message("→ Obs: ", obs_start, " to ", obs_end)
    message("→ Overlap: ", overlap_start, " to ", overlap_end)
  }
  
  # Apply window to both objects
  sim_aligned = window(sim, start = overlap_start, end = overlap_end)
  obs_aligned = window(obs, start = overlap_start, end = overlap_end)
  
  return(list(sim = sim_aligned, obs = obs_aligned))
}





