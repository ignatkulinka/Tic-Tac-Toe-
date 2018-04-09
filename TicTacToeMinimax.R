#Ignat Kulinka
#Tic Tac Toe Assignment

convertNAtoLocations <- function(state) {
  #convert NA -> board locations
  for (i in 1:length(state))
  {
    if (is.na(state[i]))
      state[i] <- i
  }
  return(state)
}

display <- function(state) {
  #convert NA to slot numbers
  state <- convertNAtoLocations(state)
  
  #print the board
  cat("\n ")
  cat(state[1:3], sep = " | ")
  cat("\n---+---+---")
  cat("\n ")
  cat(state[4:6], sep = " | ")
  cat("\n---+---+---")
  cat("\n ")
  cat(state[7:9], sep = " | ")
  cat("\n")
  
}

getOmove <- function(state) {
  #convert NA -> board locations
  for (i in 1:length(state))
  {
    if (is.na(state[i]))
      state[i] <- i
  }
  
  #internal empty moveset
  moves <- list(o = c())
  
  #get the move
  repeat {
    x <- readline("Where does O want to play?: ")
    
    #check if spot is not taken (its still a number)
    if (any(state[as.numeric(x)] == 1:9))
    {
      print("Valid Move!")
      
      #record the move
      moves$o <- append(moves$o, as.numeric(x))
      break
      
    } else{
      print("Invalid Move!")
      
    }
  }
  return(moves$o)
}

getXmove <- function(state) {
  #convert NA -> board locations
  for (i in 1:length(state))
  {
    if (is.na(state[i]))
      state[i] <- i
  }
  #internal empty moveset
  moves <- list(x = c())
  
  #get the move
  repeat {
    x <- readline("Where does X want to play?: ")
    
    #check if spot is not taken (its still a number)
    if (any(state[as.numeric(x)] == 1:9))
    {
      print("Valid Move!")
      
      #record the move
      moves$x <- append(moves$x, as.numeric(x))
      break
      
    } else{
      print("Invalid Move!")
      
    }
  }
  return(moves$x)
}

update <- function(state, who, pos) {
  state <- convertNAtoLocations(state)
  
  if (state[pos] == "x" || state[pos] == "o")
  {
    print("Invalid Move!")
  } else{
    state[pos] <- who
    return(state)
  }
  
}

check_winner <- function(state) {
  #convert NA to slot numbers
  for (i in 1:length(state))
  {
    if (is.na(state[i]))
      state[i] <- i
  }
  
  winningPositions <- list(c(1, 2, 3),
                           c(4, 5, 6),
                           c(7, 8, 9),
                           c(1, 4, 7),
                           c(2, 5, 8),
                           c(3, 6, 9),
                           c(1, 5, 9),
                           c(3, 5, 7))
  
  positions <- list()
  
  #make a list of where the x's are and o's
  for (i in 1:length(state))
  {
    if (state[i] == "x") {
      positions$x <- append(positions$x, i)
      
    }
    if (state[i] == "o") {
      positions$o <- append(positions$o, i)
    }
    
  }
  #print(positions)
  
  
  
  for (i in 1:length(winningPositions)) {
    #"x" wins
    if (all(winningPositions[[i]] %in% positions$x))
    {
      print("!!! x wins !!!")
      return(list(
        winner = TRUE,
        x = TRUE,
        o = FALSE,
        tie = FALSE
      ))
    }
    
    #"o" wins
    if (all(winningPositions[[i]] %in% positions$o)) {
      print("!!! o wins !!!")
      return(list(
        winner = TRUE,
        x = FALSE,
        o = TRUE,
        tie = FALSE
      ))
    }
  }
  
  #tie since there are no more available spots to play
  if ((length(positions$o) + length(positions$x)) == 9) {
    print("!!! its a tie !!!")
    return(list(
      winner = FALSE,
      x = FALSE,
      o = FALSE,
      tie = TRUE
    ))
  }
  
  #default state with no winner
  return(list(
    winner = FALSE,
    x = FALSE,
    o = FALSE,
    tie = FALSE
  ))
}

#how many people playing?
#is ai first?
numPlayers <- function() {
  repeat {
    x <- readline(prompt = "How many players? (Input 1 or 2): ")
    
    #AI game
    if (x == "1") {
      repeat {
        z <-
          readline(prompt = "Do you want the computer to go first? (y/n): ")
        
        if (z == "y") {
          return(list(twoPlayerGame = FALSE,
                      aiFirst = TRUE))
        }
        
        if (z == "n") {
          return(list(twoPlayerGame = FALSE,
                      aiFirst = FALSE))
        }
      }
    }
    
    #two-player game
    if (x == "2")
    {
      return(list(twoPlayerGame = TRUE, aiFirst = FALSE))
    }
  }
}

#check if the board is empty
isBoardEmpty <- function(state) {
  if (any(state == "x") || any(state == "o")) {
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

#check if the number of x's is the same as o's
doesXmatchO <- function(state) {
  state <- convertNAtoLocations(state)
  moves <- stateToMoves(state)
  
  if (length(moves$x) == length(moves$o))
  {
    return(TRUE)
  } else{
    return(FALSE)
  }
}

computer_turn <- function(state) {
  state <- convertNAtoLocations(state)
  moves <- stateToMoves(state)
  
  if (isBoardEmpty(state) || doesXmatchO(state))  {
    #cat("\n X move")
    player <<- "x"
    opponent <<- "o"
    findBestMove(state)
  } else {
    #cat("\n O move")
    player <<- "o"
    opponent <<- "x"
    findBestMove(state)
  }
}

areMovesLeft <- function(state) {
  #convert NA -> numbered locations
  state <- convertNAtoLocations(state)
  
  #taken slots: "x", "o"
  for (i in 1:length(state))
  {
    if (any(state[i] == 1:9))
    {
      #numbered slots left -> still moves to make
      return(TRUE)
    }
  }
  return(FALSE)
}

stateToMoves <- function(state) {
  moves <- list(x = c(), o = c())
  
  for (i in 1:length(state)) {
    if (state[i] == "x") {
      moves$x <- append(moves$x, i)
    }
    
    if (state[i] == "o") {
      moves$o <- append(moves$o, i)
    }
  }
  return(moves)
}

switchXtoO <- function(state) {
  state <- convertNAtoLocations(state)
  for (i in 1:length(state))  {
    if (state[i] == "x")  {
      state[i] <- "o"
    } else if (state[i] == "o") {
      state[i] <- "x"
    }
  }
  return(state)
}

evaluateState <- function(state) {
  state <- convertNAtoLocations(state)
  moves <- stateToMoves(state)
  
  if (player == "x") {
    playerLabel <- moves$x
    opponentLabel <- moves$o
  }
  else{
    playerLabel <- moves$o
    opponentLabel <- moves$x
  }
  
  winningPositions <- list(c(1, 2, 3),
                           c(4, 5, 6),
                           c(7, 8, 9),
                           c(1, 4, 7),
                           c(2, 5, 8),
                           c(3, 6, 9),
                           c(1, 5, 9),
                           c(3, 5, 7))
  
  for (i in 1:length(winningPositions)) {
    if (all(winningPositions[[i]] %in% playerLabel)) {
      return(10)
    }
    else if (all(winningPositions[[i]] %in% opponentLabel)) {
      return(-10)
    }
  }
  return(0)
}


minimax <- function(state, depth, whosMove) {
  state <- convertNAtoLocations(state)
  score <- evaluateState(state)
  if ((score == 10) || score == -10) {
    return(score - depth)
  }
  if (areMovesLeft(state) == FALSE) {
    #print("i only got here")
    return(0 - depth)
  }
  
  if (whosMove == player) {
    #print("i also got here")
    bestScore <- -1000
    
    for (i in 1:length(state)) {
      if (any(state[i] == 1:9)) {
        state[i] <- player
        
        depth <- depth + 1
        bestScore <- max(bestScore, minimax(state, depth, opponent))
        
        state[i] <- i
      }
    }
    
    return(bestScore)
    
  }
  else{
    #print("and here")
    bestScore <- 1000
    
    for (i in 1:length(state)) {
      if (any(state[i] == 1:9)) {
        state[i] <- opponent
        
        depth <- depth + 1
        bestScore <- min(bestScore, minimax(state, depth, player))
        
        state[i] <- i
      }
    }
    return(bestScore)
  }
}


findBestMove <- function(state) {
  state <- convertNAtoLocations(state)
  bestValue = -1000
  bestMove <- c()
  
  for (i in 1:length(state)) {
    if (any(state[i] == 1:9)) {
      state[i] <- player
      
      moveValue <- minimax(state, 1, opponent)
      #print(moveValue)
      state[i] <- i
      
      if (moveValue > bestValue) {
        bestValue <- moveValue
        bestMove <- i
      }
      
    }
  }
  #print(bestValue)
  return(bestMove)
}

play <- function() {
  # determine game conditons: 1 or 2 players. If computer plays, is it player 1 or 2.
  
  state <- rep(NA, 9)
  
  gameType <- list(twoPlayerGame = FALSE, aiFirst = FALSE)
  
  gameState <-
    list(
      winner = FALSE,
      x = FALSE,
      o = FALSE,
      tie = FALSE
    )
  
  moves <- list(x = c(), o = c())
  
  gameType <- numPlayers()
  #print(gameType)
  while (!gameState$winner & !gameState$tie) {
    #show the initially empty board
    display(state)
    
    #check gameType for how the game will run
    if (gameType$twoPlayerGame == TRUE) {
      #two player game
      moves$x <- getXmove(state)
    } else if (gameType$twoPlayerGame == FALSE &&
               gameType$aiFirst == TRUE) {
      #ai game & ai goes first (x)
      moves$x <- computer_turn(state)
    } else if (gameType$twoPlayerGame == FALSE &&
               gameType$aiFirst == FALSE) {
      #ai game BUT ai goes second (o)
      moves$x <- getXmove(state)
    }
    
    #DEBUG MOVE LIST
    #print(moves)
    
    #update board/state with the "x" move
    state <- update(state, "x", moves$x[length(moves$x)])
    
    #check if "x" has won
    gameState <- check_winner(state) # if x wins - quit loop
    
    #break out of the while loop early if x won
    if (gameState$winner == TRUE || gameState$tie == TRUE) {
      break
    }
    
    # o's turn
    # first display the board
    display(state)
    
    #check the gameType to see who gets to make the next "o" move
    if (gameType$twoPlayerGame == TRUE) {
      #regular two human player game, get the move from console
      moves$o <- getOmove(state)
    } else if (gameType$twoPlayerGame == FALSE &&
               gameType$aiFirst == TRUE) {
      #ai game and the ai already went (ai is "x")
      moves$o <- getOmove(state)
    } else if (gameType$twoPlayerGame == FALSE &&
               gameType$aiFirst == FALSE) {
      #ai game but the ai has not went yet, (ai is "o")
      moves$o <- computer_turn(state)
    }
    
    #update board
    state <- update(state, "o", moves$o[length(moves$o)])
    
    #display the board as well
    #display(state)
    
    #check if "o" has won or tied with the last move
    gameState <- check_winner(state) # if o wins - quit loop
  }
  
  #display the final board
  display(state)
}
