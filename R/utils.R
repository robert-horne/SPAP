######### HELPER FUNCTIONS #########
check.integer <- function(x) {
  x == round(x)
}
readinteger <- function(mssg)
{
  n <- readline(prompt=mssg)
  if(!grepl("^[[:punct:]|0-9]+$",n))
  {
    return(readinteger(mssg))
  }
  if(n==0) return(0)

  return(as.integer(n))
}


######### MATRIX CREATOR #########
createMatrix <- function(type="i", n, const, min, max) {
  if (type=="r") { #random matrix creation

    if(hasArg(min)&hasArg(max)&hasArg(n)){
      A<-matrix(floor(runif(n*n,min=(min-1),max=(max+1))),nrow = n,ncol = n,byrow = TRUE)
      diag(A)<-Inf
      return(A)
    }
    print("Random mode started.")
    n <- readinteger("Please choose a size for your matrix, an integer N(to produce a NxN matrix): (enter 0 to quit)")
    if(n<=0) stop("Processed exited.")
    zmin <- readinteger("Please a minimum integer range: (enter 0 to quit)")
    if(zmin<=0) stop("Processed exited.")
    zmax <- readinteger("Please a maximum integer range: (enter 0 to quit)")
    if(zmax<=0) stop("Processed exited.")

    A<-matrix(floor(runif(n*n,min=(zmin-1),max=(zmax+1))),nrow = n,ncol = n,byrow = TRUE)
    diag(A)<-Inf

    return(A)

  }else if(type=="f") {

    if(hasArg(const)&hasArg(n)){
      A<-matrix(rep(const,n*n),nrow = n,ncol = n,byrow = TRUE)
      diag(A)<-Inf
      return(A)
    }

    print("Flat mode started.")
    n <- readinteger("Please choose an integer N (to produce a NxN matrix): (enter 0 to quit)")
    if(n<=0) stop("Processed exited.")
    z <- readinteger("Please choose an number to fill the arc distances with: (enter 0 to quit)")
    if(z<=0) stop("Processed exited.")
    A<-matrix(rep(z,n*n),nrow = n,ncol = n,byrow = TRUE)
    diag(A)<-Inf
    return(A)

  }else if (type=="i") { #interactive matrix creation

    print("Interactive mode started.")
    n <- readinteger("Choose an integer number of nodes: (run createMatrix(\"r\") to randomly generate; enter 0 to quit)")
    if(n<=0) stop("Processed exited.")
    if(n>10) stop("Interactive mode is not supported for graphs with more than 10 nodes.")

    A<-matrix(rep(Inf,n*n),nrow = n,ncol = n,byrow = TRUE)

    t <- readinteger("Create directed graph? 1=Yes;0=No. Default is undirected.")
    directedFlag = FALSE
    if(t==1) directedFlag = TRUE

    #Start querying user for input.
    for (i in 1:n) { #rows
      for(j in 1:n){ #columns
        if(i!=j){ #test 1
          if(!((!directedFlag) & (i>j))) { #Do not edit negation logic, consider 4 possibilities and desired results.
            a <- readinteger(paste("Please enter an integer value for the arc ", i, "-", j, ": (Enter -1 if no connection; 0 to stop interactive input)" ,sep = " "))
            if(a==-1) {
              next()
            }else if (a!=0) {
              A[i,j] <- a
              if(!directedFlag) A[j,i] <- a
              rm(a)
            } else {
              stop("Processed exited.")
            }
          }
        }
      }
    }

    return(A)

  }
}

