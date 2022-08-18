library(digest)

#Part 1. Making a simple block 
#Each block needs six elements: the index of the block, the time it was created, 
#the data, the index of the previous hash, the proof of work, 
#and an element for the new hash. 
#Elements are arbitrary, just needed to define structure of a block.
my_block <- list(index = 1, timestamp = "2021-08-25 22:22:22 IDT",
                 data = "Simple_Block" , previous_hash = 0, proof = 8, new_hash=NULL)
print(my_block)

#Part 2A. Making a hash using the function digest. The function takes in two arguments, 
#the object to be encrypted and the algorithm to create the encrypted message.
Hash <- digest("FinTech","sha256")
print(Hash)

# Hash of the previous hash
print(digest(Hash,"sha256"))

#Part 2B. Allocating the hash of the block created 
#to the new hash element from the list in Part 1
print(digest(my_block, "sha256"))
HashMyBlock <- digest(my_block,"sha256")

my_block$new_hash <- HashMyBlock

#Part 3. Proof of work. Creating a task that needs to be solved as proof of work.
#A function that takes the last proof as an input and outputs the new proof.
#The while loop increments the proof number until the condition is satisfied

proof_of_work <- function(last_proof, difficulty){
  proof <- 3
  HashProof <- digest(proof,"sha256")
  zero <- paste(rep("0", difficulty), collapse="")
  while(substr(HashProof, 1, difficulty)!=zero){
    proof <- proof + 1
    HashProof <- digest(proof,"sha256")
  }
  return(proof)
}

print(proof_of_work(2,2))
print(proof_of_work(3,3))

#Part 4 Adding new blocks. Take in previous block and solve proof of work in part 3. 
#Create new block with same structure as part 1, using result of proof of work.
# Hash the new block and return it
??genesis
mine <- function(previous_block, difficulty){
  new_proof <- proof_of_work(previous_block$proof, difficulty)
  new_block <- list(number = previous_block$number + 1,
                      timestamp = Sys.time(),
                      data = paste("I am big block", previous_block$number + 1),
                      parent_hash = previous_block$hash,
                      proof = new_proof,
                      new_hash = NULL)
  # add hash 
  new_block$new_hash <- digest(new_block, "sha256")
  
  return(new_block)
}
print(mine(my_block,3))

#Part 5. Create a block, append new blocks onto the blockchain variable,
# change difficulty, and determine time difference to append new blocks.
# Plot blocks made over time

blockchained <- function(nblocks, difficulty=3) {
  start <- Sys.time()
  
  first_block = list(index = 1, 
                     timestamp = Sys.time(), 
                     data = "Big Blocks", 
                     parent_hash = .5,
                     proof = 0.5,
                     new_hash = NULL)   

  first_block$new_hash = digest(first_block,"sha256")
  
  blockchain <- list(first_block)
  
    for (i in 2:nblocks){
      blockchain[[i]] <- mine(blockchain[[i-1]], difficulty) 
    }
    end <- Sys.time() 
    return(end-start)
  }


print(blockchained(3,3))

Number_of_blocks <- c(5,25,50,75)

time <- mapply(blockchained, Number_of_blocks)

plot(Number_of_blocks,time, ylab = "Time in Seconds", xlab = "Number of Blocks",
     main = "Blocks created vs Time to Create")


#6 From online references.
proof_of_work1 = function(block1, difficulty) {
  block$nonce = 0
  hash1 = digest(block, "sha256")
  zero = paste(rep("0", difficulty), collapse="")
  while(substr(hash, 1, difficulty) != zero) {
    block$nonce = block$nonce + 1 
    hash = digest(block, "sha256")  
  }
  return(list(hash = hash, nonce = block$nonce))
}

minenow <- function(previous_block, difficulty = 3, genesis = FALSE){
  
  if (genesis) {
    # define genesis block
    new_block <-  list(number = 1,
                       timestamp = Sys.time(),
                       data = "BIG BLOCK",
                       parent_hash = "0")  
  } else {
    # create new block
    new_block <- list(number = previous_block$number + 1,
                      timestamp = Sys.time(),
                      data = paste0("I'm block ", previous_block$number + 1),
                      parent_hash = previous_block$hash)
  }
  # add nonce with PoW
  new_block$nonce <- proof_of_work1(new_block, difficulty)$nonce
  # add hash 
  new_block$hash <- digest(new_block, "sha256")
  return(new_block)
}

blockchained1 = function(difficulty, nblocks) {
  # mine genesis block
  block_genesis = mine(NULL, difficulty, TRUE)   
  # first block is the genesis block
  blockchain <- list(block_genesis)
  
  if (nblocks >= 2) {
    # add new blocks to the chain
    for (i in 2:nblocks){
      blockchain[[i]] <- minenow(blockchain[[i-1]], difficulty) 
    }
  }
  
  return(blockchain)
}


blockchained1(3,3)
