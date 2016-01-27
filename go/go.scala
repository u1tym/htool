object go {

	def main( args : Array[ String ] ) : Unit = {

		var cont : Boolean = true
		while( cont ) {

			print( "> " )
			val strs = readLine.split( " " )


			cont = if ( strs( 0 ) == "quit" ) false else true
		}
	}

}
