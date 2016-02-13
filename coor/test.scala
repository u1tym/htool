
object Main {

	def main( args : Array[ String ] ) : Unit = {

		var p1 = new Coordinate( "35N130E" )

		var dir = new Direction( 0, 10 )

		var p2 = p1 * dir

		p2.print

	}

}

