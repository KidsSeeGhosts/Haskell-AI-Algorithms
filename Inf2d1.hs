-- Inf2d Assignment 1 2018-2019
-- Matriculation number:s1724559
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = [(2,1),(2,2),(2,3),(2,4),(2,5),(2,6)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth= 21 
-- Why did you choose this number?
-- YOUR ANSWER GOES HERE
--From (1,1) to (6,6) (the maximum distance to travel) with maximum number of obstacles (15) that still allow a path to destination,
--the maximum number of nodes in this branch is the size of the grid (36) minus the maximum number of obstacles (15)
--Therefore length of the maximum possible branch is (36-15)= 21 which is equal to (initial node + depth=20)
--So in iterDeepSearch a depth of 21 is never searched

-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] =  []
next branch = filter (\z -> (fst (head z)) <=6 && (fst (head z)) >=1 && (snd (head z)) <=6 && (snd (head z)) >=1) --gets rid of out of bounds nodes (off-grid)
	(filter (\y -> notElem (head y) (tail y)) ---gets rid of duplicate nodes
	(filter (\x -> notElem (head x) badNodesList) --gets rid of bad nodes
	[(fst n + 1, snd n):branch, (fst n - 1, snd n):branch, (fst n, snd n + 1):branch, (fst n, snd n - 1):branch])) --appends list of branches
	where
		n = head branch --head of a branch is a node

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode --returns true if the current node and destination are the same node


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch _ _ [] _ = Nothing -- when queue is empty return nothing
breadthFirstSearch destination next (x:xs) exploredList
	| (checkArrival destination (head x)) == True = Just x -- head node is destination then return the branch
	| (notElem (head x) exploredList) = breadthFirstSearch destination next (xs++(next x)) (exploredList++[(head x)]) --if node hasn't been expanded before, it gets added to end of the queue to be expanded and to explored list
	| otherwise = breadthFirstSearch destination next xs exploredList --if node has been expanded before then don't add it to expansion queue

-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
depthFirstSearch _ _ [] _ = Nothing
depthFirstSearch destination next (x:xs) exploredList
	| (checkArrival destination (head x)) == True = Just x --if head node of branch is destination return the branch
	| (notElem (head x) exploredList) = depthFirstSearch destination next ((next x)++xs) (exploredList++[(head x)]) --add expanded node branch to front of queue and to explored list
	| otherwise = depthFirstSearch destination next xs exploredList --if node has already been expanded move along the branch expansion queue


-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch _ _ [] _ = Nothing
depthLimitedSearch destination next (x:xs) d
	| (checkArrival destination (head x)) == True = Just x --if head node of branch is destination return it
	| ((length x) /= d+1) = depthLimitedSearch destination next ((next x)++xs) d --expand when length of branch isn't d+1
	| (length x == d+1) && (length xs > 0) = depthLimitedSearch destination next xs d --branch is too long, go back a branch
	| otherwise = Nothing
	--length d+1 chosen because d+1 is length of branch = (original node + d number of nodes each representing an expansion of the tree)


-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next node d 
	| d >= maxDepth = Nothing --Returns nothing if the depth is equal to max depth or greater (my maxDepth is 21)
	| (depthLimitedSearch destination next [[node]] d) /= Nothing = (depthLimitedSearch destination next [[node]] d) --if depthLimitedSearch finds a solution, this solution is returned
	| (depthLimitedSearch destination next [[node]] d) == Nothing = iterDeepSearch destination next node (d+1) --if depthLimitedSearch returns Nothing, increase the depth to be searched


-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan position destination = abs (fst position - fst destination) + abs (snd position - snd destination) --absolute difference in x + absolute difference in y of the nodes

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch _ _ _ [] _ = Nothing
bestFirstSearch destination next heuristic (x:xs) exploredList 
	| (checkArrival destination (head x)) == True = Just x
	| (notElem (head x) exploredList) = bestFirstSearch destination next heuristic (orderBranches ((next x)++xs) heuristic) (exploredList++[(head x)]) -- if node not explored, expand branch then re-order branches including the new branches in order of lowest manhattan distance
	| (elem (head x) exploredList) = bestFirstSearch destination next heuristic (orderBranches xs heuristic) exploredList --if node already explored, reorder branches in order of lowest manhattan distance

-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch _ _ _ _ [] _ = Nothing
aStarSearch destination next heuristic cost (x:xs) exploredList 
	| (checkArrival destination (head x)) == True = Just x
	| (notElem (head x) exploredList) = aStarSearch destination next heuristic cost (orderAStarBranches ((next x)++xs) heuristic) (exploredList++[(head x)]) --if node not explored, expand branch then re-order branches including the new branches in order of lowest (cost+manhattan distance)
	| (elem (head x) exploredList) = aStarSearch destination next heuristic cost (orderAStarBranches xs heuristic) exploredList --order branches in order of (cost+manhattan distance)

-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost [] = 0
cost branch = length branch - 1 --subtracting one since the initial node in a branch costs nothing


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game
	| (terminal game) && (checkWin game 1) = 1 --rewards max player a score of 1
	| (terminal game) && (checkWin game 0) = -1 --rewards min player a score of -1
	| otherwise = 0 --rewards 0 in case of a draw

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player 
	| (terminal game) = eval game --if game has finished, evaluate it and reward points
	| (maxPlayer player) = maximum [ minimax x (switch player) | x<-((moves game player))] -- if max player then finding max MINIMAX(RESULT(s,a)) where a∈Actions(s)
	| (minPlayer player) = minimum [ minimax y (switch player) | y<-((moves game player))] -- if min player then finding min MINIMAX(RESULT(s,a)) where a∈Actions(s)


-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Game->Player->Int
alphabeta game player = trackAlphaBeta game player (-2) 2 --alpha starts at -2 and beta starts at 2, new helper function used to recurse and track alpha and beta

-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game
	| (terminal game) && (checkWin game 1) = 1 --rewards max player a score of 1
	| (terminal game) && (checkWin game 0) = 1 --rewards min player a score of 1
	| otherwise = 0 --rewards 0 in case of a draw


-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

alphabetaWild:: Game->Player->Int
alphabetaWild game player = trackAlphaBetaWild game player (-2) 2 --alpha starts at -2 and beta starts at 2, calls new wild helper function used to recurse and track alpha and beta


-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player =undefined



-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores

--Helper functions for Best First Search to help order branches in order of lowest to highest manhattan distances
orderBranches::[Branch]->(Node->Int)->[Branch] --orders the branches from lowest manhattan distance to highest using SortBy
orderBranches branches heuristic = sortBy (orderPairs heuristic) branches

orderPairs :: (Node->Int) -> Branch -> Branch -> Ordering --creates ordering type for sortBy in orderBranches to work
orderPairs heuristic first second --compares consecutive branches by their head node's manhattan distance
			| (heuristic (head second)) < (heuristic (head first)) = GT
			| (heuristic (head first)) < (heuristic (head second)) = LT
			| (heuristic (head second)) == (heuristic (head first)) = EQ



--Helper functions for A* Search to consider cost when ordering the branches
orderAStarBranches::[Branch]->(Node->Int)->[Branch] --orders the branches from lowest (manhattan distance+cost) to highest using SortBy
orderAStarBranches branches heuristic = sortBy (orderAStarPairs heuristic) branches

orderAStarPairs :: (Node->Int) -> Branch -> Branch -> Ordering --creates ordering type for sortBy in orderAStarBranches to work
orderAStarPairs heuristic first second --similar to orderPairs except compares branches by their cost in addition to their head node's manhattan distance
			| ((heuristic (head second)) + (cost second)) < ((heuristic (head first)) + (cost first)) = GT
			| ((heuristic (head first)) + (cost first)) < ((heuristic (head second)) + (cost second)) = LT
			| ((heuristic (head second)) + (cost second)) == ((heuristic (head first)) + (cost first)) = EQ



--Helper functions for Alpha Beta in order to keep track of alpha, beta and current max and min values
trackAlphaBeta::Game->Player->Int->Int->Int --this function initialises values of max and min player
trackAlphaBeta game player alpha beta
	| (maxPlayer player) = maxPlayerValue (moves game player) player alpha beta (-2) -- Finding the max player value from the max player's next possible moves, -2 is initial value for max player
	| (minPlayer player) = minPlayerValue (moves game player) player alpha beta (2)  --Finding the min player value from the min player's next possible moves, 2 is initial valye for min player
			
maxPlayerValue::[Game]->Player->Int->Int->Int->Int --finding the max value by recursing through all possible values for max player
maxPlayerValue [] player alpha beta value = value
maxPlayerValue (x:xs) player alpha beta value
	| (terminal x) = eval x  --if game has finished, evaluate it and reward points
	| (newMaxValue>=beta) = newMaxValue --prune as this value is worse for min player so won't ever be chosen by min player
	| otherwise = maxPlayerValue xs player (max alpha newMaxValue ) beta newMaxValue -- finds the next max value and updates alpha
			where
				newMaxValue = max value (trackAlphaBeta x 0 alpha beta) --fetches new max value by comparing current value and value from recursing trackAlphaBeta with min player
			
			
minPlayerValue::[Game]->Player->Int->Int->Int->Int --finding the min value by recursing through all possible values for min player
minPlayerValue [] player alpha beta value = value
minPlayerValue (x:xs) player alpha beta value
	| (terminal x) = eval x --if game has finished, evaluate it and reward points
	| (newMinValue<=alpha) = newMinValue --prune as this value is worse for max player so won't ever be chosen by max player
	| otherwise = minPlayerValue xs player alpha (min beta newMinValue) newMinValue --finds the next min value and updates beta
			where
				newMinValue = min value (trackAlphaBeta x 1 alpha beta) --fetches new min value by comparing current value and value from recursing trackAlphaBeta with max player



--Helper functions for Wild Alpha Beta in order to keep track of alpha, beta and current max and min values. Almost identical to non wild alpha beta helper functions except for using wild functions and adjusting the score for min player winning
trackAlphaBetaWild::Game->Player->Int->Int->Int --this function initialises values of max and min player
trackAlphaBetaWild game player alpha beta
	| (terminal game) = (evalWild game)*(if (maxPlayer player) then (-1) else 1) --if it's max players turn then result of evalWild is multiplied by -1 since min player sent game into terminal state and must be rewarded -1 instead of 1
	| (maxPlayer player) = maxPlayerValueWild (movesWild game player) player alpha beta (-2) --Finding the max player value from the max player's next possible moves, -2 is initial value for max player
	| (minPlayer player) = minPlayerValueWild (movesWild game player) player alpha beta (2)  --Finding the min player value from the min player's next possible moves, 2 is initial valye for min player
					
maxPlayerValueWild::[Game]->Player->Int->Int->Int->Int --finding the wild max value by recursing through all possible values for max player
maxPlayerValueWild [] player alpha beta value = value
maxPlayerValueWild (x:xs) player alpha beta value
	| (newMaxValueWild>=beta) = newMaxValueWild --prune as this value is worse for min player so won't ever be chosen by min player
	| otherwise = maxPlayerValueWild xs player (max alpha newMaxValueWild) beta newMaxValueWild -- finds the next wild max value and updates alpha
		where
			newMaxValueWild = max value (trackAlphaBetaWild x (switch player) alpha beta) --fetches new wild max value by comparing current value and value from recursing trackAlphaBetaWild with min player
				
minPlayerValueWild::[Game]->Player->Int->Int->Int->Int --finding the wild min value by recursing through all possible values for min player
minPlayerValueWild [] player alpha beta value = value
minPlayerValueWild (x:xs) player alpha beta value
	| (newMinValueWild<=alpha) = newMinValueWild --prune as this value is worse for max player so won't ever be chosen by max player
	| otherwise = minPlayerValueWild xs player alpha (min beta newMinValueWild) newMinValueWild --finds the next wild min value and updates beta
		where
			newMinValueWild = min value (trackAlphaBetaWild x (switch player) alpha beta) --fetches new wild min value by comparing current value and value from recursing trackAlphaBeta Wildwith max player