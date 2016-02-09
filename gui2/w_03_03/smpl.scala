import javafx.application.{ Application => Application }
import javafx.stage.Stage
import javafx.scene.Scene
import javafx.scene.layout.BorderPane
import javafx.scene.control.Label
import javafx.scene.control.TextField
import javafx.scene.control.Button
import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.fxml.FXML
import javafx.fxml.FXMLLoader

object smpl {

	def main( args : Array[ String ] ) : Unit = {

		Application.launch( classOf[ smpl ], args : _* )

	}

}

class smpl extends Application {

	override def start( sts : Stage ) : Unit = {

		var pane : BorderPane = FXMLLoader.load( getClass.getResource( "smpl.fxml" ) )

		var scene = new Scene( pane, 300, 150 )
		sts.setScene( scene )

		sts.show()

	}

	def handle( act : ActionEvent ) = {
		println( "click!!" )
	}

}

class evtController {

	@FXML var label1 : Label = _

	@FXML var field1 : TextField = _
	@FXML var btn1   : Button = _

	@FXML private def doAction( evt : ActionEvent ) = {

		label1.setText( "clicked" )

		println( "clicked!!" )

	}

}


