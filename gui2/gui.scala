import javafx.application.{ Application => Application }
import javafx.stage.Stage

object smpl {

	def main( args : Array[ String ] ) : Unit = {

		Application.launch( classOf[ smpl ], args : _* )

	}

}

class smpl extends Application {

	override def start( sts : Stage ) : Unit = {

		sts.show()

	}

}

