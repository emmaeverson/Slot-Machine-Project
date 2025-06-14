--------run slot machine symbols-------------
get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

-----skeleton of final score function, adding prize to symbols------
*notice that all # are just code comments instead of real code 
# = places you need to make code still
  
  if ( # Case 1: all the same) {
  prize <- # look up the prize
  } else if ( # Case 2: all bars  {
    prize <- # assign $5
    } else {
      # count cherries
      prize <- # calculate a prize
    }3

# count diamonds
# double the prize if necessary
--------------------------------------------

------parts of final score------------------
#case 1: all the same
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
#case 2: all bars
  bars <- all(symbols %in% c("B","BB","BBB"))
#look up prize
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
               "B" = 10, "C" = 10, "0" = 0)
  prize <- unname(payouts[symbols[1]])
#assign $5
  prize <- 5
#count cherries
  cherries <- sum(symbols == "C")
#count diamonds
  diamonds <- sum(symbols == "DD")
#calculate a prize
  prize <- c(0,2,5)[cherries+1]
    #[] tells you which value to extract from data frame *in this case c(0,2,5). can't just write cherries because the 
    #   first option is 0 so need to add 1. this makes c=0 correspond with $0, c=1 -> $2, and c=2 -> $5
#double the prize if necessary
  prize *2 ^ diamonds
  
------------final score function-----------
    same <- symbols[1] == symbols[2] && symbols[2] == symbols[3] 
    bars <- symbols %in% c("B", "BB", "BBB")
    #put same and bars first so that the full code is easier to read
    #bars is written as all(bars) to make it clear what its testing for, could just write bars but all(bars) 
    #makes it obvious that its checking not just for there to be a bar but for all of them to be bars
    
    if (same) {
    prize <-   payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
                            "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[symbols[1]])
    } else if (all(bars))  {
      prize <- prize <- 5
      } else {
        cherries <- sum(symbols == "C")
        prize <- prize <- c(0,2,5)[cherries+1]
      }

  diamonds <- sum(symbols == "DD")
  prize *2 ^ diamonds
  
-------------wrap in function-------------
    score <- function (symbols) {
      # identify case
      same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
      bars <- symbols %in% c("B", "BB", "BBB")
      # get prize
      if (same) {
        payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
                     "B" = 10, "C" = 10, "0" = 0)
        prize <- unname(payouts[symbols[1]])
      } else if (all(bars)) {
        prize <- 5
      } else {
        cherries <- sum(symbols == "C")
        prize <- c(0, 2, 5)[cherries + 1]
      }
      # adjust for diamonds
      diamonds <- sum(symbols == "DD")
      prize * 2 ^ diamonds
    }
  
----------- play function-------------
    play <- function() {
      symbols <- get_symbols()
      print(symbols)
      score(symbols)
    }
##remember that you cannot just copy play function from here, need to include code for defining symbols and making score function
##score function does not work on its own because the play function is where symbols is defined as get_symbols()
  
#if this function is copied to a new object (e.g. one_play <- play()) R will ignore the symbols when manipulating one_play
  
-----------modified and final play functions
    play <- function() {
      symbols <- get_symbols()
      prize <- score(symbols)
      attr(prize, "symbols") <- symbols
      prize
    }
##lets attributes (symbols) stay with the final prize when copied to a new object
  
  play <- function() {
    symbols <- get_symbols()
    structure(score(symbols), symbols = symbols)
  }
#same as above function but simplified using structure()
  
--------------function to make clean display (gets rid of " " around each slot symbol and adds $ for prize amount)
    slot_display <- function(prize){
      # extract symbols
      symbols <- attr(prize, "symbols")
      # collapse symbols into single string
      symbols <- paste(symbols, collapse = " ")
      # combine symbol with prize as a regular expression
      # \n is regular expression for new line (i.e. return or enter)
      string <- paste(symbols, prize, sep = "\n$")
      # display regular expression in console without quotes
      cat(string)
    }
#now slot_display is a way to manually clean up the play function, use it like this:
    slot_display(play())
    
---------------new print method for slots class
    print.slots <- function(x, ...) {
      slot_display(x)
    }
#now R will automatically use slot_display function to display objects of class slots
    play <- function() {
      symbols <- get_symbols()
      structure(score(symbols), symbols = symbols, class = "slots")
    }
#adding (class = "slots") to structure automatically assigns all outputs of play as having the class "slots"
  #with this you don't have to manually do slot_display(play()), R will automatically do the slot_display function
    #inside the print function to bring the result onto the console window
  
    
    
    
    
-----Calculating probability of a weighted die roll (easy learning before implementing in slots)
#use expand.grid to quickly come up with all possible combinations of vectors
  rolls <- expand.grid(die, die)
  
#adds a 3rd column that shows sum of dice rolls
  rolls$value <- rolls$Var1 + rolls$Var2
  
#add 2 more columns for probability of each die (first make lookup table for probs then add columns to table)
  prob <- c("1" = 1/8, "2" = 1/8, "3" = 1/8, "4" = 1/8, "5" = 1/8, "6" = 3/8)
  rolls$prob1<-prob[rolls$Var1]
  rolls$prob2<-prob[rolls$Var2]
  
#combine probabilities to make probability of both die then sum to get expected average value
  rolls$probfinal<-(rolls$prob1 * rolls$prob2)
  sum(rolls$value * rolls$probfinal)
  
------calculate probability of slot machine rolls #check notes in above section for explanation if needed
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  
  prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)
  
  combos$prob1<-prob[combos$Var1]
  combos$prob2<-prob[combos$Var2]
  combos$prob3<-prob[combos$Var3]
  
  combos$prob<-(combos$prob1 * combos$prob2 * combos$prob3)
  
#create a new empty column so that a for loop can fill it with outputs
  combos$prize <- NA
  
#create a for loop  
  for (i in 1:nrow(combos)) {
    symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3]) #this will make 'symbols' the combination of whatever row i is 
    combos$prize[i] <- score(symbols) #this will run the score function for that row and save it in the newly made prize column
  }
    #running this will repeat the code between the { } for every single row in the table
  
#calculate expected value of prize
  sum(combos$prize * combos$prob)
  #answer comes out as 0.538014 which means the machine gives out 53 cents on the dollar
  
  
  
------------------version of score that handles wild diamonds:
    score <- function(symbols) {
      diamonds <- sum(symbols == "DD")
      cherries <- sum(symbols == "C")

      slots <- symbols[symbols != "DD"] #removes DD from symbols used in if else because they'll be treated as whatever makes highest
            #cont. for example, DD B B and B B B would both have all Bs with the line above so the else if (same) would yield the same results
      same <- length(unique(slots)) == 1
      bars <- slots %in% c("B", "BB", "BBB")
      # assign prize
      if (diamonds == 3) {
        prize <- 100
      } else if (same) {
        payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                     "B" = 10, "C" = 10, "0" = 0) #removed "DD" from payouts because first option if deals with all same DD
        prize <- unname(payouts[slots[1]]) #slots instead of symbols because its the update to exclude DD
      } else if (all(bars)) {
        prize <- 5
      } else if (cherries > 0) {
        prize <- c(0, 2, 5)[cherries + diamonds + 1] #diamonds count as cherries when cherries > 0 so it can be added like a cherry
      } else {
        prize <- 0
      } 
      prize * 2^diamonds #double for each diamond
      }
#with this new function you can rerun the previous code (only the for loop part is necessary) to find probability 
      #you'll get 0.934356 and this makes it match the manufacturers claim for the Montreal slot machines in the example
      
  --------------vectorized code (estimate payout rate using simulation)
#rewrite of get_symbols that generates n slot combinations and returns then as an n x 3 matrix
  get_many_symbols <- function(n) {
    wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    vec <- sample(wheel, size = 3 * n, replace = TRUE, #difference on this line is * n
                  prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
    matrix(vec, ncol = 3) #adding this line makes it into a matrix
  }

#rewrite play to take a parameter, n, and return n prizes in a data frame   
  play_many <- function(n) {
    symb_mat <- get_many_symbols(n = n)
    data.frame(w1 = symb_mat[,1], w2 = symb_mat[,2],
               w3 = symb_mat[,3], prize = score_many(symb_mat))

#rewrite score to be vectorized, gets rid of if loop
    #seems way longer because every option needs to written out but because they are all vectorized R can run each one a lot
    #faster than running the loop for every single one
    # symbols should be a matrix with a column for each slot machine window
    score_many <- function(symbols) {
     
   # Step 1: Assign base prize based on cherries and diamonds ---------
   ## Count the number of cherries and diamonds in each combination
      cherries <- rowSums(symbols == "C") #rowSums instead of just sums because made into matrix during get_many_symbols
      diamonds <- rowSums(symbols == "DD")
      
   ## Wild diamonds count as cherries
      prize <- c(0, 2, 5)[cherries + diamonds + 1]
      prize[!cherries] <- 0 #makes prize zero if there are no cherries because otherwise the above would count 1 diamond with
                            #no cherries as 1 cherry (diamonds can only count if there is a real cherry)
   
   # Step 2: Change prize for combinations that contain three of a kind
      same <- symbols[, 1] == symbols[, 2] &
        symbols[, 2] == symbols[, 3] #written differently than before because now getting from matrix not just one line, leaving [x, y] 
                                      #x blank allows it to be whatever row is currently being looked at, labels that row "same"
      
      payoffs <- c("DD" = 100, "7" = 80, "BBB" = 40,
                   "BB" = 25, "B" = 10, "C" = 10, "0" = 0) #same as before just named payoffs instead of payouts to not override
      
      prize[same] <- payoffs[symbols[same, 1]] #makes the prize for anything labelled same
      
  # Step 3: Change prize for combinations that contain all bars ------
      bars <- symbols == "B" | symbols == "BB" | symbols == "BBB" # \ behaves the same as &, same as bars <- slots %in% c("B", "BB", "BBB")
                                                                  # in first one
      all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same #
      prize[all_bars] <- 5
      
  # Step 4: Handle wilds ---------------------------------------------
      ## combos with two diamonds
      two_wilds <- diamonds == 2
      
      ### Identify the nonwild symbol
      one <- two_wilds & symbols[, 1] != symbols[, 2] & #already established diamonds as separate now need to find the odd one
                                                        #out by going through each column to find which one is different
        symbols[, 2] == symbols[, 3]
      two <- two_wilds & symbols[, 1] != symbols[, 2] &
        symbols[, 1] == symbols[, 3]
      three <- two_wilds & symbols[, 1] == symbols[, 2] &
        symbols[, 2] != symbols[, 3]
      
      ### Treat as three of a kind
      prize[one] <- payoffs[symbols[one, 1]] #targets the odd one out and counts diamonds as that for 3 of a kind
      prize[two] <- payoffs[symbols[two, 2]] 
      prize[three] <- payoffs[symbols[three, 3]]
      
      ## combos with one wild
      one_wild <- diamonds == 1
      
      ### Treat as all bars (if appropriate)
      wild_bars <- one_wild & (rowSums(bars) == 2) #can't be treated same as 3 of a kind 
      prize[wild_bars] <- 5
      
      ### Treat as three of a kind (if appropriate)
      one <- one_wild & symbols[, 1] == symbols[, 2] #same theory as identifying nonwild for 2 diamonds
      two <- one_wild & symbols[, 2] == symbols[, 3]
      three <- one_wild & symbols[, 3] == symbols[, 1]
      
      prize[one] <- payoffs[symbols[one, 1]] #exact same as previous three of a kind
      prize[two] <- payoffs[symbols[two, 2]]
      prize[three] <- payoffs[symbols[three, 3]]
      
  # Step 5: Double prize for every diamond in combo ------------------
      unname(prize * 2^diamonds) #need to unname prize because labelled them "all_bars", "one", "two", etc.
    }
    
    