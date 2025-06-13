#run slot machine
get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

score <- function(symbols) {
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  slots <- symbols[symbols != "DD"] 
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0) 
    prize <- unname(payouts[slots[1]]) 
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    prize <- c(0, 2, 5)[cherries + diamonds + 1] 
  } else {
    prize <- 0
  } 
  prize * 2^diamonds 
}

slot_display <- function(prize){
  symbols <- attr(prize, "symbols")
  symbols <- paste(symbols, collapse = " ")
  string <- paste(symbols, prize, sep = "\n$")
  cat(string)
}

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}


#calculate probability of slot machine rolls (payout rate)
wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")

prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

combos$prob1<-prob[combos$Var1]
combos$prob2<-prob[combos$Var2]
combos$prob3<-prob[combos$Var3]

combos$prob<-(combos$prob1 * combos$prob2 * combos$prob3)

combos$prize <- NA

for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3]) 
  combos$prize[i] <- score(symbols)
}

sum(combos$prize * combos$prob)


#vectorized version
get_many_symbols <- function(n) {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  vec <- sample(wheel, size = 3 * n, replace = TRUE, prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
  matrix(vec, ncol = 3) 
}

score_many <- function(symbols) {
  cherries <- rowSums(symbols == "C")
  diamonds <- rowSums(symbols == "DD")
  
  prize <- c(0, 2, 5)[cherries + diamonds + 1]
  prize[!cherries] <- 0 
  
  same <- symbols[, 1] == symbols[, 2] &
    symbols[, 2] == symbols[, 3]
  
  payoffs <- c("DD" = 100, "7" = 80, "BBB" = 40,
               "BB" = 25, "B" = 10, "C" = 10, "0" = 0) 
  
  prize[same] <- payoffs[symbols[same, 1]] 
  
  bars <- symbols == "B" | symbols == "BB" | symbols == "BBB"
  all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same #
  prize[all_bars] <- 5
  
  two_wilds <- diamonds == 2
  
  one <- two_wilds & symbols[, 1] != symbols[, 2] & 
    symbols[, 2] == symbols[, 3]
  two <- two_wilds & symbols[, 1] != symbols[, 2] &
    symbols[, 1] == symbols[, 3]
  three <- two_wilds & symbols[, 1] == symbols[, 2] &
    symbols[, 2] != symbols[, 3]
  
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]] 
  prize[three] <- payoffs[symbols[three, 3]]
  
  one_wild <- diamonds == 1

  wild_bars <- one_wild & (rowSums(bars) == 2) 
  prize[wild_bars] <- 5
  
  one <- one_wild & symbols[, 1] == symbols[, 2]
  two <- one_wild & symbols[, 2] == symbols[, 3]
  three <- one_wild & symbols[, 3] == symbols[, 1]
  
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  unname(prize * 2^diamonds)
}

play_many <- function(n) {
  symb_mat <- get_many_symbols(n = n)
  data.frame(w1 = symb_mat[,1], w2 = symb_mat[,2],
             w3 = symb_mat[,3], prize = score_many(symb_mat))