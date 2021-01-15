
# removeDirContents
# a function that will remove all files of the given directory
# NOTE this is an unrecoverable delete
# inputs
#   dir - the directory to be cleared. Default=. (current workspace)
#   pattern - pattern to match files to be deleted
removeDirContents <- function(dir='.', pattern='*') {
	files <- list.files(dir, pattern=pattern)
	for (f in files) {
	  print(f)
		file.remove(paste0(dir, '/', f))
	}
}
