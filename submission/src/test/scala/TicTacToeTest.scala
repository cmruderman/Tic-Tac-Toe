import hw.tictactoe._
import Solution._

class TicTacToeTest extends org.scalatest.FunSuite {



////////////////////////////////////      isFinished and getWinner Tests       //////////////////////////////////////////////////////
	val antiDX = Map(((0,0), O), ((0,1), X), ((0,2), X),
				     ((1,0), O), ((1,1), X), ((1,2), O), 
				     ((2,0), X), ((2,1), O), ((2,2), X))

	val antiDXGame = Solution.createGame(O, 3, antiDX)

		test ("isFinished with 3 dim and antiD X") {
			assert(antiDXGame.isFinished()==true)
		}
		test ("getWinner with 3 dim and antiD X") {
			assert(antiDXGame.getWinner()==Some(X))
		}

	val regDX = Map(((0,0), X), ((0,1), X), ((0,2), X),
				    ((1,0), O), ((1,1), X), ((1,2), O), 
				    ((2,0), O), ((2,1), O), ((2,2), X))

	val regDXGame = Solution.createGame(O, 3, regDX)

		test ("isFinished with 3 dim and regD X") {
			assert(regDXGame.isFinished()==true)
		}
		test ("getWinner with 3 dim and regD X") {
			assert(regDXGame.getWinner()==Some(X))
		}

	val regDO=  Map(((0,0), O), ((0,1), X), ((0,2), O), ((0,3), X),
					((1,0), X), ((1,1), O), ((1,2), X), ((1,3), X),
					((2,0), X), ((2,1), O), ((2,2), O), ((2,3), O),
					((3,0), O), ((3,1), X), ((3,2), O), ((3,3), O))

	val regDOGame = Solution.createGame(X, 4, regDO)
		test ("isFinished with 4 dim and regD O") {
			assert(regDOGame.isFinished()==true)
		}
		test ("getWinner with 4 dim and regD O") {
			assert(regDOGame.getWinner()==Some(O))
		}

	val antiDO =Map(((0,0), X), ((0,1), X), ((0,2), O), ((0,3), O),
					((1,0), X), ((1,1), X), ((1,2), O), ((1,3), X),
					((2,0), X), ((2,1), O), ((2,2), O), ((2,3), O),
					((3,0), O), ((3,1), X), ((3,2), X), ((3,3), O))
	val antiDOGame = Solution.createGame(O, 4, antiDO)
		test ("isFinished with 4 dim and antiD O") {
			assert(antiDOGame.isFinished()==true)
		}
		test ("getWinner with 4 dim and antiD O") {
			assert(antiDOGame.getWinner()==Some(O))
		}

	val horizX =Map(((0,0), O), ((0,1), X), ((0,2), O), ((0,3), O), ((0,4), X),
			     	((1,0), O), ((1,1), X), ((1,2), O), ((1,3), X), ((1,4), O),
				 	((2,0), X), ((2,1), X), ((2,2), X), ((2,3), X), ((2,4), X),
				 	((3,0), O), ((3,1), O), ((3,2), O), ((3,3), X), ((3,4), X),
					((4,0), X), ((4,1), X), ((4,2), O), ((4,3), O), ((4,4), O))

	val horizXGame = Solution.createGame(O, 5, horizX)
		test ("isFinished with 5 dim and X wins horiz") {
			assert(horizXGame.isFinished()==true)
		}
		test ("getWinner with 5 dim and X wins horiz") {
			assert(horizXGame.getWinner()==Some(X))
		}


	val nW = Map(((0,0), X), ((0,1), O), ((0,2), X), ((0,3), O),
			     ((1,0), X), ((1,1), O), ((1,2), X), ((1,3), O),
				 ((2,0), O), ((2,1), X), ((2,2), O), ((2,3), X),
				 ((3,0), O), ((3,1), X), ((3,2), O), ((3,3), X))

	val noWin = Solution.createGame(O, 4, nW)
		test ("isFinished with 4 dim and no winner") {
			assert(noWin.isFinished()==false)
		}
		test ("getWinner with 4 dim and no winner") {
			assert(noWin.getWinner()==None)
		}


	val horizO =Map(((0,0), O), ((0,1), X), ((0,2), X), ((0,3), O), ((0,4), O), ((0,5), X),
			     	((1,0), O), ((1,1), O), ((1,2), O), ((1,3), X), ((1,4), O), ((1,5), X), 
				 	((2,0), X), ((2,1), X), ((2,2), O), ((2,3), O), ((2,4), X), ((2,5), X), 
				 	((3,0), O), ((3,1), X), ((3,2), X), ((3,3), X), ((3,4), O), ((3,5), O), 
					((4,0), X), ((4,1), O), ((4,2), O), ((4,3), O), ((4,4), X), ((4,5), X), 
					((5,0), O), ((5,1), O), ((5,2), O), ((5,3), O), ((5,4), O), ((5,5), O))

	val horizOGame = Solution.createGame(O, 6, horizO)
		test ("isFinished with 6 dim and O wins horiz") {
			assert(horizOGame.isFinished()==true)
		}
		test ("getWinner with 6 dim and O wins horiz") {
			assert(horizOGame.getWinner()==Some(O))
		}

	val vertX =Map(((0,0), O), ((0,1), X), ((0,2), X), ((0,3), O), ((0,4), O), ((0,5), X),
			       ((1,0), O), ((1,1), O), ((1,2), O), ((1,3), X), ((1,4), O), ((1,5), X), 
				   ((2,0), X), ((2,1), X), ((2,2), O), ((2,3), O), ((2,4), X), ((2,5), X), 
				   ((3,0), O), ((3,1), X), ((3,2), X), ((3,3), X), ((3,4), O), ((3,5), X), 
				   ((4,0), X), ((4,1), O), ((4,2), O), ((4,3), O), ((4,4), X), ((4,5), X), 
				   ((5,0), O), ((5,1), O), ((5,2), O), ((5,3), O), ((5,4), O), ((5,5), X))

	val vertXGame = Solution.createGame(O, 6, vertX)

		test ("isFinished with 6 dim and X wins vert") {
			assert(vertXGame.isFinished()==true)
		}
		test ("getWinner with 6 dim and X wins vert") {
			assert(vertXGame.getWinner()==Some(X))
		}

	val sixXDiag = Map(((0,0), X), ((0,1), O), ((0,2), O), ((0,3), O), ((0,4), X), ((0,5), O),
				       ((1,0), O), ((1,1), X), ((1,2), O), ((1,3), O), ((1,4), X), ((1,5), O), 
					   ((2,0), O), ((2,1), X), ((2,2), X), ((2,3), O), ((2,4), O), ((2,5), X), 
					   ((3,0), O), ((3,1), O), ((3,2), O), ((3,3), X), ((3,4), O), ((3,5), O), 
					   ((4,0), X), ((4,1), O), ((4,2), X), ((4,3), X), ((4,4), X), ((4,5), O), 
					   ((5,0), O), ((5,1), X), ((5,2), O), ((5,3), O), ((5,4), O), ((5,5), X))

	val sixX = Solution.createGame(O, 6, sixXDiag)

		test ("isFinished with 6 dim and X wins diag") {
			assert(sixX.isFinished()==true)
		}
		test ("getWinner with 6 dim and X wins diag") {
			assert(sixX.getWinner()==Some(X))
		}	

	val emty = Solution.createGame(O, 3, Map())
		test ("isFinished with empty") {
			assert(emty.isFinished()==false)
		}
		test ("getWinner with empty") {
			assert(emty.getWinner()==None)
		}

	////////////////////////////////////      nextBoards test       //////////////////////////////////////////////////////

	val mMap =  Map(((0,0), X), ((0,2), X),
				    ((1,0), O), ((1,2), O), 
				    ((2,0), O), ((2,1), O), ((2,2), X))

	val g = Solution.createGame(X, 3, mMap)

	val brd = g.nextBoards()

	test ("nextBoards tester 1") {
			assert(brd.length == ((g.dimension)*(g.dimension) - mMap.size))
		}

	test("The solution object must be defined") {
	    val obj : hw.tictactoe.MinimaxLike = Solution
	  }

	////////////////////////////////////      miniMax tests       //////////////////////////////////////////////////////

	val mMax =  Map(((0,0), X), ((0,2), X),
				    ((1,0), O), ((1,2), O), 
				    ((2,0), O), ((2,1), O), ((2,2), X))
	val miniM = Solution.createGame(X, 3, mMax)
	val mnM= miniM.nextBoards()

	test ("nextBoards tester 2") {
			assert(mnM.length == ((miniM.dimension)*(miniM.dimension) - mMax.size))
		}
	test ("miniMax test1") {
			assert(minimax(miniM)==Some(X))
		}

	val mMax2 =Map(((0,0), O), ((0,1), X), ((0,2), X), ((0,3), O), ((0,4), O), ((0,5), X),
			     	((1,0), O), ((1,1), O), ((1,2), O), ((1,3), X), ((1,4), O), ((1,5), X), 
				 	((2,0), X), ((2,1), X), ((2,2), O), ((2,3), O), ((2,4), X), ((2,5), X), 
				 	((3,0), O), ((3,1), X), ((3,2), X), ((3,3), X), ((3,4), O), ((3,5), O), 
					((4,0), X), ((4,1), O), ((4,2), O), ((4,3), O), ((4,4), X), ((4,5), X), 
					((5,0), O), ((5,1), O), ((5,3), O), ((5,4), O), ((5,5), O))

	val miniM2 = Solution.createGame(O, 6, mMax2)

	test ("miniMax test2") {
			assert(minimax(miniM2)==Some(O))
		}

	val mMax3 =  Map(((0,0), X), ((0,2), O),
				    ((1,0), X), ((1,1), X), 
				    ((2,0), O))

	val miniM3 = Solution.createGame(O, 3, mMax3)
	val mnM3= miniM3.nextBoards()


	test ("miniMax test3") {
			assert(minimax(miniM3)==Some(X))
		}

	val nW1 = Map(((0,0), X), ((0,1), O), ((0,2), X), ((0,3), O),
			     ((1,0), X), ((1,1), O), ((1,2), X), ((1,3), O),
				 ((2,0), O), ((2,1), X), ((2,2), O), ((2,3), X),
				 ((3,0), O), ((3,3), X))


	val miniM4 = Solution.createGame(X, 4, nW1)


	test ("miniMax test4") {
			assert(minimax(miniM4)==None)
		}

	test ("miniMax test5") {
			assert(minimax(createGame(X,3,Map()))==None)
		}

	val noWinr = Map((0,0) -> X, (1,0) -> O, (0,1) -> X, (1,1) -> O, (1,2) -> X)


	val miniM6 = Solution.createGame(O, 3, noWinr)


	test ("miniMax test6") {
			assert(minimax(miniM6)==None)
		}

	val gameA1 = createGame(O, 3, Map())
	val gameB1 = createGame(X, 3, Map())
	val gameC1 = createGame(O, 2, Map())

	test("test 1") {
			assert(minimax(gameA1) == None)
			assert(minimax(gameB1) == None)
			assert(minimax(gameC1) == Some(O))
	}
	val board2 = Map((0,0)->O)
	val gameA2 = createGame(O, 3, board2)
	val gameB2 = createGame(X, 3, board2)

	test("test 2") {
		assert(minimax(gameA2) == Some(O))
		assert(minimax(gameB2) == None)
	}
	val board3 = Map((1,1)->O)
	val gameA3 = createGame(X, 3, board3)
	val gameB3 = createGame(O, 3, board3)

	test("test 3") {
		assert(minimax(gameA3) == None)
		assert(minimax(gameB3) == Some(O))
	}

	val board4 = Map((0,0)-> O, (2,2)-> X)
	val gameA4 = createGame(O, 3, board4)
	val gameB4 = createGame(X, 3, board4)	

	test("test 4") {
		assert(minimax(gameA4) == Some(O))
		assert(minimax(gameB4) == Some(X))
	}

	val board5 = Map((0,0)-> X, (0,1)-> O, (0,2)-> X, (1,0)-> X, (1,1)-> X, (1,2)-> O, (2,0)-> O, (2,1)-> X, (2,2)-> O)
	val game5A = createGame(X, 3, board5)
	val game5B = createGame(O, 3, board5)

	test("test 5") {
		assert(minimax(game5A) == None)
		assert(minimax(game5B) == None)
	}

	val game6 = createGame(O, 3, Map((1,0)-> X))
	test("test 6") {
		assert(minimax(game6) == None)
	}

	val board7 = Map((0,0)-> X, (1,0)->X, (2,0)->X, (0,1)->X, (1,1)->X, (2,1)->X)
	val gameA7 = createGame(X, 3, board7)
	val gameB7 = createGame(O, 3, board7)

	test("test 7") {
		assert(minimax(gameA7) == Some(X))
		assert(minimax(gameB7) == Some(X))
	}

}