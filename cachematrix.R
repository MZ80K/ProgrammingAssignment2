# Return the inverse of a matrix. Calculate the inverse on the first
# access and cache the result. Return the cached inverse on subsequent
# accesses.
#

# makeCacheMatrix(mtrx=matrix())
# Provide interface to matrix to set/get its contents and its inverse

makeCacheMatrix<-function(mtrx=matrix()) {
	# Helper functions that provide interface to matrix:
	# set - assigns contents to matrix and sets inverse to null
	# get - returns contents of matrix
	# setinv - caches inverse of matrix for future use
	# getinv - returns inverse of matrix if cached, else null
	mtrx.inv<-NULL
	set<-function(m) {
		mtrx<<-m
		mtrx.inv<<-NULL		# Use <<- to assign mtrx.inv in parent
	}								# environment
	get<-function() mtrx
	setinv<-function(inv) mtrx.inv<<-inv
	getinv<-function() mtrx.inv
	list(set=set, get=get,setinv=setinv,getinv=getinv)
}


# cacheSolve(mtrx,...)
# Return the inverse of a matrix, either calculated or cached

cacheSolve<-function(mtrx,...) {
	# Return the inverse of matrix 'matrx'.
	# On first access, calculate inverse and cache it.
	# On subsequent accesses, return the cached inverse.
	# Assume that mtrx is always invertible.
	inv<-mtrx$getinv()		# Ask for cached inverse
	if(!is.null(inv)) {		# If it exists, return it
		message("getting cached inverse")
		return(inv)
	}
	# Else, calculate the inverse and cache it:
	m<-mtrx$get()				# Get the matrix
	message("calculating inverse and caching")
	inv<-solve(m,...)			# Calculate the inverse
	mtrx$setinv(inv)			# Cache the inverse
	inv							# Return the inverse
}

