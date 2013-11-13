
#rm(ls=list())
mini <- FALSE

#============================== Setup for running on Gauss... ==============================#

args <- commandArgs(TRUE)

cat("Command-line arguments:\n")
print(args)

####
# sim_start ==> Lowest possible dataset number
###

###################
sim_start <- 1000
###################

if (length(args)==0){
  sim_num <- sim_start + 1
  set.seed(121231)
} else {
  # SLURM can use either 0- or 1-indexing...
  # Lets use 1-indexing here...
  sim_num <- sim_start + as.numeric(args[1])
  sim_seed <- (762*(sim_num-1) + 121231)
}

cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))

# Find r and s indices:
s = 5
r = 50
s_index = ceiling((sim_num - sim_start)/r)
r_index = sim_num - sim_start - r*(s_index - 1)

#============================== Run the simulation study ==============================#

# Load packages:
library(BH)
library(bigmemory.sri)
library(bigmemory)
library(biganalytics)

# I/O specifications:
datapath <- "/home/pdbaines/data"
outpath <- "output/"

# mini or full?
if (mini){
  rootfilename <- "blb_lin_reg_mini"
} else {
  rootfilename <- "blb_lin_reg_data"
}


# Filenames:
infilename <- paste0(rootfilename,".txt")
backingfilename <- paste0(rootfilename,".bin")
descriptorfilename <- paste0(rootfilename,".desc")

# Set up I/O stuff:
infile <- paste(datapath,infilename,sep="/")
backingfile <- paste(datapath,backingfilename,sep="/")
descriptorfile <- paste(datapath,descriptorfilename,sep="/")

# Attach big.matrix :
data = attach.big.matrix( dget(descriptorfile), backingpath = datapath)

# write.table(data, 'data_out.txt')
# data = read.table( infile, sep=',')

# Remaining BLB specs:
n = nrow(data)
gamma = 0.7
b= floor(n^gamma)
tt = paste0(outpath, 'n_txt.txt')
# write.table(n, tt)
# Reset simulation seed:
set.seed(s_index)

# Extract the subset:
row.select = sort(sample(n, b))
# print(seed)
# data selected without replacement.
data.select = data[row.select,]
data.select = data.frame(data.select)
names(data.select)[ncol(data.select)] = 'Y'

# Reset simulation seed:
as.numeric(Sys.time())-> t
set.seed((t - floor(t)) * 1e8 -> seed)

# Bootstrap dataset:
probs = seq(1, b)/b
boots = rmultinom(1, size = n, prob = probs )

# Fit lm:
fit = lm(formula = Y~.+0, data = data.select, weights = boots)

# Output file:
outfile = paste0(outpath,"coef_",sprintf("%02d",s_index), "_", sprintf("%02d",r_index),".txt")

# Save estimates to file:
write.table(coef(fit), outfile, na="", )


