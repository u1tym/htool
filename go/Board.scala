class Board {

	var ro : Int = 9
	var stone = Array.ofDim[ Int ]( ro + 2, ro + 2 )

	for( x <- 0 to ( ro + 1 ) ) {
		stone( x )( 0 ) = 9
		stone( x )( ro + 1 ) = 9
	}

	for( y <- 0 to ( ro + 1 ) ) {
		stone( 0 )( y ) = 9
		stone( ro + 1 )( y ) = 9
	}

	def set( x : Int, y : Int, color : Int ) = {
		stone( x )( y ) = color
	}

	def look : Unit = {
		for( y <- 1 to ro ) {
			for( x <- 1 to ro ) {
				val v = stone( x )( y ) match {
							case  1 => "o"
							case -1 => "o"
							case  _ => "+"
				}
				print( v )
			}
			println( "" )
		}
		return ()
	}

}

