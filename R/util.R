
# removeDirContents
# a function that will remove all files of the given directory
# NOTE this is an unrecoverable delete
# inputs
#   dir - the directory to be cleared. Default=. (current workspace)
removeDirContents <- function(dir='.') {
	files <- list.files(dir)
	for (f in files) {
	  print(f)
		file.remove(paste0(dir, '/', f))
	}
}
