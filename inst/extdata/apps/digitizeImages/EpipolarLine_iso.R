## code comes from end of "server.R" file in inst/extdata/apps/digizeImages directory

		# Find epipolar line
		if(!is.null(json_list$find_epipolar_line)){

			out$slope <- c()
			out$intercept <- c()
			out$cubic <- c()

			# Read calibration coefficients to matrix
			cal_coeffs <- matrix(as.numeric(unlist(json_list$cal_coeffs)), nrow=11, byrow=TRUE)

			# Read distortion related info to matrices
			if(json_list$undistort_params[1] != ''){
				undistort_params <- matrix(suppressWarnings(as.numeric(unlist(json_list$undistort_params))), nrow=2, byrow=TRUE)
				distort_params <- matrix(suppressWarnings(as.numeric(unlist(json_list$distort_params))), nrow=2, byrow=TRUE)
				img_size <- matrix(as.numeric(unlist(json_list$img_size)), nrow=2, byrow=TRUE)
			}

			for(i in 1:length(json_list$all_views)){

				# Skip current view
				if(i == json_list$current_view){
					if(json_list$undistort_params[1] == ''){
						out$slope <- c(out$slope, "NA")
						out$intercept <- c(out$intercept, "NA")
					}else{
						out$cubic <- c(out$cubic, rep("NA", 8))
					}
					next
				}

				# Get file path
				full_shape_fpath <- paste0(json_list$prev_wd, '/', json_list$all_views[i])
				#full_shape_fpath <- paste0(json_list$prev_wd, '/', json_list$all_views[json_list$current_view])

				# Skip non-existant or empty files
				if(!file.exists(full_shape_fpath) || file.info(full_shape_fpath)$size == 0) next

				# Get shapes
				shapes_list <- XML4R2list(full_shape_fpath)$shapes

				# Skip if required shape data not found
				if(!json_list$shape_type %in% names(shapes_list)) next

				# Landmark from view other than current
				landmark <- rep(NA, 2)
				
				if(json_list$shape_type == 'landmarks.pixel'){
				
					# Skip if landmark is not in matrix
					if(!json_list$landmark_name %in% rownames(shapes_list$landmarks.pixel)) next

					# Get landmark
					landmark <- shapes_list$landmarks.pixel[json_list$landmark_name, ]
				}

				# Skip if landmark is NA
				if(is.na(landmark[1])) next
				
				# Check if there are undistortion parameters
				if(json_list$undistort_params[1] == ''){

					# Find epipolar line (no distortion case)
					dlt_epipolar_line <- dltEpipolarLine(landmark, cal_coeffs[, c(i, json_list$current_view)])

					out$slope <- c(out$slope, dlt_epipolar_line$m)
					out$intercept <- c(out$intercept, dlt_epipolar_line$b)

				}else{

					# Find Epipolar curve as Bezier (distortion present)
					#print(list(p=landmark, cal.coeff=cal_coeffs, 
					#	undistort.coeff=undistort_params, distort.coeff=distort_params,
					#	img.size=img_size, views=c(i, json_list$current_view)))
					ebezier <- epipolarBezier(p=landmark, cal.coeff=cal_coeffs, 
						undistort.coeff=undistort_params, distort.coeff=distort_params,
						img.size=img_size, views=c(i, json_list$current_view))
					
					#print(ebezier)
					
					bpts <- c(ebezier$p[[1]][1], ebezier$p[[2]][1], ebezier$p[[1]][2], ebezier$p[[2]][2],
						ebezier$p[[1]][3], ebezier$p[[2]][3], ebezier$p[[1]][4], ebezier$p[[2]][4])
					
					out$cubic <- c(out$cubic, bpts)
				}
			}
			
			if(length(out$slope) == 0) out$slope <- "NA"
			if(length(out$intercept) == 0) out$slope <- "NA"
			if(length(out$cubic) == 0) out$cubic <- "NA"
		}

		out_str <- listToJSONStr(out, direct=TRUE)

		#cat(out_str);cat("\n")

		out_str
	})
})


# Is there a way to just have it project a horizontal line at the y coordinate from the landmark on one image onto the next image? 

	# This should be a line with slope = o and intercept is the y coordinate value from the first image
	
	# So, what does it need to do?
#
# 	1. Check to see if landmark has been digitized on another this or the other view
# 	a. If yes, extract y coordinate (need to figure out where this is stored...) and project a line on the current image with a slope=0 and intercept=y coord
# 	b. If no, do nothing.
# 	2. When moving onto the next landmark, stop displaying this horizontal line and repeat the process for the new landmark.
#
#
#
	
	
	
	