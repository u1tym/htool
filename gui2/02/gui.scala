import javafx.application.Application
import javafx.stage.Stage
import javafx.scene.Scene
import javafx.scene.layout.BorderPane
import javafx.scene.control.Label
import javafx.scene.control.TextField
import javafx.scene.control.Button

object smpl {

	def main( args : Array[ String ] ) : Unit = {

		Application.launch( classOf[ smpl ], args : _* )

	}

}

class smpl extends Application {

	override def start( stage : Stage ) : Unit = {

		var label  = new Label( "sample" )
		var button = new Button( "Click" )
		var text   = new TextField()

		var pane = new BorderPane()
		pane.setTop( label )
		pane.setCenter( text )
		pane.setBottom( button )

		var scene = new Scene( pane, 320, 240 )

		stage.setScene( scene )

		stage.show()

	}

}

