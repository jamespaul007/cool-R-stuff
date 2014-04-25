install.packages(c("deSolve", "RCurl","stringr","RSQLite","DBI"))
install.packages("~/Downloads/RGoogleTrends_0.2-1.tar.gz", repos = NULL, type = "source")

library(deSolve)
library(RGoogleTrends)
library(RCurl)  	# For getURL() and curl handler / cookie / google login
library(stringr)	# For str_trim() to trip whitespace from strings
# Download the RGoogleTrends package from http://www.omegahat.org/RGoogleTrends/RGoogleTrends_0.2-1.tar.gz


# Code to download trends data courtesy of Christoph Riedl of Northeastern university
# http://christophriedl.net/2013/08/22/google-trends-with-r/

# Google account settings
username <- "YOUR_NAME@gmail.com"
password <- "YOUR_PASSWORD"

username <- "jamespaul007@gmail.com"
password <- "Jimbo007"

# URLs
loginURL 		<- "https://accounts.google.com/accounts/ServiceLogin"
authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"
trendsURL 		<- "http://www.google.com/trends?"

############################################
## This gets the GALX cookie which we need to pass back with the login form
############################################
getGALX <- function(curl) {
  txt = basicTextGatherer()
  curlPerform( url=loginURL, curl=curl, writefunction=txt$update, header=TRUE, ssl.verifypeer=FALSE )
  
  tmp <- txt$value()
  
  val <- grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], val = TRUE)
  strsplit(val, "[:=;]")[[1]][3]
  
  return( strsplit( val, "[:=;]")[[1]][3]) 
}


############################################
## Function to perform Google login and get cookies ready
############################################
gLogin <- function(username, password) {
  ch <- getCurlHandle()
  
  ans <- (curlSetOpt(curl = ch,
                     ssl.verifypeer = FALSE,
                     useragent = getOption('HTTPUserAgent', "R"),
                     timeout = 60,         
                     followlocation = TRUE,
                     cookiejar = "./cookies",
                     cookiefile = ""))
  
  galx <- getGALX(ch)
  authenticatePage <- postForm(authenticateURL, .params=list(Email=username, Passwd=password, GALX=galx, PersistentCookie="yes", continue="http://www.google.com/trends"), curl=ch)
  
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  
  if(getCurlInfo(ch)$response.code == 200) {
    print("Google login successful!")
  } else {
    print("Google login failed!")
  }
  return(ch)
}

#function to query a search string
gQuery <-function(username, password, queryString){
  ch <- gLogin( username, password )
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  res <- getForm(trendsURL, q=queryString, content=1, export=1, graph="all_csv", curl=ch)
  str(res)
  # Check if quota limit reached
  if( grepl( "You have reached your quota limit", res ) ) {
    stop( "Quota limit reached; You should wait a while and try again lateer" )
  }
  
  # Parse resonse and store in CSV
  # We skip ther first 5 rows which contain the Google header; we then read 503 rows up to the current date
  x <- try( read.table(text=res, sep=",", col.names=c("Week", "TrendsCount"), skip=32, nrows=513) )
  return(x$TrendsCount)
}

myspaceData <-gQuery(username, password, "myspace")
plot(myspaceData)
fbData<-gQuery(username, password, "facebook")
plot(fbData)

sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}
## Parameters
## (Just pick some numbers)
params <- c(beta=0.5, gamma=0.5)

## vector of time steps

times <- seq(1, 52, by=1)

## Initial conditions
IC <- c(S=99, I=1, R=0)

## Solving ODE
Output <- lsoda(y=IC, times=times, func=sir, parms=params)
plot(Output)


#function that measures distance between solution of SIRmodel
#and incidence data in Region 1
sirDist <- function(x){
  #x should have 3 entries
  x[1]=beta
  x[2]=gamma
  x[3]=99 #Initial number of infected O to 100
  
  GhostParameters <- c(b=x[1], g=x[2])
  InitialCondition <- c(S=100-.5, I=.5, R=0)
  
  Ghost<-lsoda(InitialCondition,times,sir,GhostParameters)
  
  a<-Ghost[,"I"]
  
  
  Dist<-sum((Region1-a)^2)
  
  Dist
}

#Optimize sirDist

#Bad starting value
y <- c(0.5,0.5)

X<-optim(y,sirDist)

a<-X$par

#plotting the solution of the ODE with the "optimal" parameters

OptimalParams <- c(beta=a[1], gamma=a[2])
IC <- c(S=99.5, I=0.5, R=0) # same IC as in SIRdist()


init <- c(S = 1-1e-6, I = 1e-6, 0.0)
parameters <- c(beta = 1.4247, gamma = 0.14286)
times <- seq(0, 70, by = 1)
out <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters))
out$time <- NULL

matplot(times, out, type = "l", xlab = "Time", ylab = "Susceptibles and Recovereds", main = "SIR Model", lwd = 1, lty = 1, bty = "l", col = 2:4)
legend(40, 0.7, c("Susceptibles", "Infecteds", "Recovereds"), pch = 1, col = 2:4)
