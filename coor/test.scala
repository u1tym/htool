
object Main {

	def main( args : Array[ String ] ) : Unit = {

		var lat = new DMST( "895959N" )
		lat.print

		var c = new Coordinate( "754030555N1355040333E" )
		c.print

		var d = new Distance( 10 )
		d *= 5

		d.print

	}

}

