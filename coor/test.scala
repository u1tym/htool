
object Main {

	def main( args : Array[ String ] ) : Unit = {

		var p1 = new Coordinate( "35N135E" )

		var dir = new Direction( degree = 45, meter = 100000  )

		var p2 = p1 * dir

		p2.print

	}

}

