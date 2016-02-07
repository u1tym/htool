import javafx.application.Application
import javafx.stage.Stage
import javafx.scene.Scene
import javafx.scene.layout.BorderPane
import javafx.fxml.FXMLLoader

//import javafx.scene.control.Label
//import javafx.scene.control.TextField
//import javafx.scene.control.Button
//import javafx.event.ActionEvent
//import javafx.event.EventHandler

object smpl {

	def main( args : Array[ String ] ) : Unit = {

		Application.launch( classOf[ smpl ], args : _* )

	}

}

class smpl extends Application {

	override def start( stage : Stage ) : Unit = {

		var pane : BorderPane = FXMLLoader.load( getClass.getResource( "gui.fxml" ) )

		var scene = new Scene( pane, 320, 240 )

		stage.setScene( scene )

		stage.show()

	}

}


