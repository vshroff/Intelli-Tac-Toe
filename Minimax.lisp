;AI VS Player Tic-Tac-To game

;SUMMARY:
; I have used the minimax Algorithm for this project and have modified some of the code in the algorithm in order to make it faster. Although this has helped me increase the speed
; of the AI, it might have costed me a potentially perfect AI player.
; My code is built through the following functions:
; CONTAINS() and GAMEOVER() are helper functions that check to see if a number exists in a list and if all the spaces in the list have been filled respectively
; UTILITY() gives a score or a value. For example, 1 for win, -1 for loss and 0 for tie
; TERMINAL-TEST returns 1 if game is over or win condition is achieved and 0 otherwise
; ACTIONS returns a list of possible actions for the AI player for each state
; Result gives the resultant state after an action is applied to the current state
; The humanInput function is another helper function that I used to ask user input 
; The Tic-Tac-To() function is the main function that invokes my game
; The most important function I have used is the MINIMAX-DECISION function which evaluates the best action from the point of view of the AI player
; This function calls the Min value function which in turn calls the Max Value function recursively
; The base case of this recursion is reached when a player reaches a terminal state during the process of the VIRTUAL game and this is when a utlity value is returned.
; The minimax decision function evaluates the best utlity from the AI's point of view for each possible action at every state
; Since the time complexity of this algorithm is huge, I have modified the algorithm in order to exit out if enough time has been elapsed and have documented this next to the code as well.

;----------------------------------------------------------------------------------------------------------------------------------------
;helper function to check if a number exists in a list
(defun CONTAINS(state num) 
(if(member num state)
	'1
	'0)	  
)

;helper function to check the gameOver condition, i.e. if all spaces have been filled
(defun GAMEOVER(state)
(setq numList (list '1 '2 '3 '4 '5 '6 '7 '8 '9 '10 '11 '12 '13 '14 '15 '16))
(setq checkVar 1)
(loop for i in state 
	do(if (eql (CONTAINS numList i) 1) (setq checkVar 0))

)
(return-from GAMEOVER checkVar);returns 1 if no numbers left

)

;----------------------------------------------------------------------------------------------------------------------------------------
(defun UTILITY(state m) ;returns 1 if M wins, -1 if loses, else 0 for tie

(if (eql (GAMEOVER state) 1) (return-from UTILITY 0))

(setq plus1 (list '1 '2 '5 '6 '9 '10 '13 '14))
(setq plus3 (list '3 '4 '7 '8))
(setq plus4 (list '1 '2 '3 '4 '5 '6 '7 '8))
(setq plus5 (list '1 '2 '5 '6))
(loop for i in state
	 for j from 0
		;All conditions for a player to WIN in a 4X4 board
		do(if (and (eql (CONTAINS plus1 (+ j 1)) 1 )(eql i (nth (+ j 1) state) ) (eql (nth (+ j 1) state) (nth (+ j 2) state))) (if (eql i m) (return-from UTILITY 1) (return-from UTILITY (- 0 1))) )
		do(if (and (eql (CONTAINS plus3 (+ j 1)) 1 )(eql i (nth (+ j 3) state) ) (eql (nth (+ j 3) state) (nth (+ j 6) state))) (if (eql i m) (return-from UTILITY 1) (return-from UTILITY (- 0 1))) )
		do(if (and (eql (CONTAINS plus4 (+ j 1)) 1 )(eql i (nth (+ j 4) state) ) (eql (nth (+ j 4) state) (nth (+ j 8) state))) (if (eql i m) (return-from UTILITY 1) (return-from UTILITY (- 0 1))) )
		do(if (and (eql (CONTAINS plus5 (+ j 1)) 1 )(eql i (nth (+ j 5) state) ) (eql (nth (+ j 5) state) (nth (+ j 10) state))) (if (eql i m) (return-from UTILITY 1) (return-from UTILITY (- 0 1))))
)
(return-from UTILITY 0)
)

;----------------------------------------------------------------------------------------------------------------------------------------
;TERMINAL-TEST returns 1 if true, 0 otherwise
(defun TERMINAL-TEST(state) ;returns 1 if TRUE 

(if (eql (GAMEOVER state) 1) (return-from TERMINAL-TEST 0))

(setq plus1 (list '1 '2 '5 '6 '9 '10 '13 '14))
(setq plus3 (list '3 '4 '7 '8))
(setq plus4 (list '1 '2 '3 '4 '5 '6 '7 '8))
(setq plus5 (list '1 '2 '5 '6))


(loop for i in state
	 for j from 0
		;All different conditions of winning in a 4x4 board
		do(if (and (eql (CONTAINS plus1 (+ j 1)) 1 )(eql i (nth (+ j 1) state) ) (eql (nth (+ j 1) state) (nth (+ j 2) state))) (return-from TERMINAL-TEST 1) )
		do(if (and (eql (CONTAINS plus3 (+ j 1)) 1 )(eql i (nth (+ j 3) state) ) (eql (nth (+ j 3) state) (nth (+ j 6) state))) (return-from TERMINAL-TEST 1) )
		do(if (and (eql (CONTAINS plus4 (+ j 1)) 1 )(eql i (nth (+ j 4) state) ) (eql (nth (+ j 4) state) (nth (+ j 8) state))) (return-from TERMINAL-TEST 1) )
		do(if (and (eql (CONTAINS plus5 (+ j 1)) 1 )(eql i (nth (+ j 5) state) ) (eql (nth (+ j 5) state) (nth (+ j 10) state)))(return-from TERMINAL-TEST 1) )
)
(return-from TERMINAL-TEST 0)

 
)

;----------------------------------------------------------------------------------------------------------------------------------------

;ACTIONS returns all possible actions that a player can take in any given state
(defun ACTIONS(state)
(setq str "A")
(setq actions (list 'A0))


	(loop for i in state 
		for j from 1
		
		if(eql i j)
		do(nconc actions (list (concatenate 'string str (write-to-string j))))
		

	)
	(return-from ACTIONS (cdr actions))
)

;----------------------------------------------------------------------------------------------------------------------------------------

;RESULT- returns the new  state after one action has been applied to the old state
(defun RESULT (state action sym)
(setq num (subseq action 1))
(setf n(parse-integer num))



	(loop for i in state 
		for j from 1
		
		do(if (eql n j) (return-from RESULT (substitute sym i state :count 1 :start (- j 1) :end j)) )

	)

(return-from RESULT state)
)



;----------------------------------------------------------------------------------------------------------------------------------------

;This is where the minimax algorithm starts
(defun MINIMAX-DECISION (state)

(setq finalact "A0")

(loop for a in (ACTIONS state)
		for j from 0
			do(setq currentState (RESULT state a 'X)) 
			do(setq finalval (MIN-VALUE currentState))
			do(if (eql finalVal 1) (return-from MINIMAX-DECISION a) (setq finalact a));if we get a one, we stop searching for more actions and return the current action
			do(if (> j 3)(return-from MINIMAX-DECISION finalact) );Adding another layer to speed up the code
)


(return-from MINIMAX-DECISION finalact)
)

(defun MAX-VALUE(state)
		(if (eql (TERMINAL-TEST state) 1) (return-from MAX-VALUE (UTILITY state 'O))) ; returns -1 if AI wins
		(setq v (- 0 1000)) ;setting v to a very small value
		
		(loop for a in (ACTIONS state)
			for j from 0
				do(setq currResult (RESULT state a 'X))
				do(setq y (MIN-VALUE currResult))
				do(if (eql y 1) (return-from MAX-VALUE 1) (if (eql (max v y) (- 0 1)) (continue) (setq v 0) ) );terminating recursion when we get a 1 here, AI won
				do(if(> j 3) (return-from MAX-VALUE v)) ;otherwise sacrificing a potentially perfect AI player for speed
		)

(return-from MAX-VALUE v)
)

(defun MIN-VALUE(state)
		(if (eql (TERMINAL-TEST state ) 1) (return-from MIN-VALUE (UTILITY state 'X)) ); returns 1 if AI wins	
		(setq v 1000); setting it to a very large value
		
		(loop for a in (ACTIONS state)
			for j from 0
				do(setq currResult (RESULT state a 'O))
				do(setq x (MAX-VALUE currResult))
				do(if (eql x (- 0 1)) (return-from MIN-VALUE 1) (if (eql (min v x) 1) (continue) (setq v 0) ) ); terminating recursion if AI won
				do(if(> j 3) (return-from MIN-VALUE v)) ;otherwise sacrificing a potentially perfect AI player for speeed
		)

(return-from MIN-VALUE v)
)

;----------------------------------------------------------------------------------------------------------------------------------------
;function to ask for human input in the main loop
(defun HumanInput()
	(print "Please enter what position (1-16) would you like to mark")
	(setq human (read))
(return-from HumanInput human)
)

;----------------------------------------------------------------------------------------------------------------------------------------
;MAIN BODY OF THE CODE
(defun TicTacToe ()

(print "Welcome to Intelli-Tac-Toe! ")

(setq check 2); checker used to check who won 
(setq Max 'X) ; X as AI
(setq Min 'O) ; 0 as Human
(setq board (list '1 'O '3 '4 'X 'X '7 '8 '9 '10 '11 '12 '13 'O '15 '16)); setting a board to SOME start state to save time for testing the code


;NOTE: 
;Please use this board state if you want to start with a completely new board, but it will take a lot of time to process
;(setq board (list '1 '2 '3 '4 '5 '6 '7 '8 '9 '10 '11 '12 '13 '14 '15 '16)) 


(print "Start Board : ")
(write board)
  
(loop

;applying human inputted action to Board/State
(setq human (HumanInput)) 
(if (or (eql (nth (- human 1) board) Max ) (eql (nth (- human 1) board) Min ))(HumanInput)(nsubstitute Min human board))
(print "Board after Human's turn: ")

(write board)

;Checking if Human Won
(if (eql (TERMINAL-TEST board) 1) (setq check 0) )
(if (eql check 0) (print "You won!"))
(if (eql check 0) (return))

;Checking if game is Over
(if (eql (GAMEOVER board) 1) (return) )

(print "Computing AI's turn, please wait this might take a couple of minutes sometimes....")

;Applying computer applied action to Board/State using Minimax Algorithm
(setq aiInput (MINIMAX-DECISION board))

(setq aiStr (substring aiInput 1 nil))
(setq ai (parse-integer aiStr))
 
(print "AI chose action :")
(write aiInput)

;(if (eql ai 0) (return)) needed?

(nsubstitute Max ai board)
(print "Board after AI's turn: ")

(write board)

;Checking to see if AI won
(if (eql (TERMINAL-TEST board) 1) (setq check 1))
(if (eql check 1) (print "AI won!"))
(if (eql check 1) (return))

;Checking if game is Over
(if (eql (GAMEOVER board) 1) (return) )

)

(print "Game over, Thank you for playing Intelli-Tac-Toe");
)


(TicTacToe) 

;----------------------------------------------------------------------------------------------------------------------------------------
















