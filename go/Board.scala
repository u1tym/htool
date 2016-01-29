class Board {

	var ro : Int = 9
	var stone = Array.ofDim[ Kind ]( ro + 2, ro + 2 )

	for( x <- 0 to ( ro + 1 ) ) {

		stone( x )( 0 ) = new Waku
		stone( x )( ro + 1 ) = new Waku

	}

	for( y <- 0 to ( ro + 1 ) ) {

		stone( 0 )( y ) = new Waku
		stone( ro + 1 )( y ) = new Waku

	}

	def set( x : Int, y : Int, color : String ) = {

		stone( x )( y ) = new Stone( color )

	}

	def look : Unit = {
		for( y <- 1 to ro ) {
			for( x <- 1 to ro ) {
				val v = stone( x )( y ) match {
							case s : Stone => if( s.color == "black" )  " *" else " o"
							case _         => " +"
				}
				print( v )
			}
			println( "" )
		}
		return ()
	}

	def check = {

		for( x <- 1 to ro ) {
			for( y <- 1 to ro ) {

				checkStone( x, y )

			}
		}
	}

	def checkStone( x : Int, y : Int ) : Unit = {
		return ()
	}


}

class Kind {
}

class Waku extends Kind {
}

class Stone( val color : String ) extends Kind {

	var group : Option[ Int ] = None

	require( color == "black" || color == "white" )
}

class Grp( val number : Int ) {

	var lst : List[ Stone ] = Nil

	def addStone( stone : Stone ) = {

		lst = stone :: lst

	}

}



