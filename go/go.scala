object go {

	def main( args : Array[ String ] ) : Unit = {

		// 盤の生成
		var board = new Board

		var cont = true
		while( cont ) {

			print( "> " )
			val strs = readLine.split( " " )

			strs( 0 ) match {

				case "look" => board.look

				case "quit" => cont = false

				case "set"  => board.set( strs( 1 ).toInt,
							  strs( 2 ).toInt,
							  strs( 3 ) )
					       board.look

				case _      =>
			}

			// cont = if ( strs( 0 ) == "quit" ) false else true
		}
	}

}
