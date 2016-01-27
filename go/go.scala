object go {

	def main( args : Array[ String ] ) : Unit = {

		// 盤の生成
		var board = new Board

		var cont : Boolean = true
		while( cont ) {

			print( "> " )
			val strs = readLine.split( " " )

			strs( 0 ) match {

				case "look" => board.look

				case "quit" => cont = false

				case "set"  => board.set( 1, 2, 1 )
				case _      =>
			}

			// cont = if ( strs( 0 ) == "quit" ) false else true
		}
	}

}
