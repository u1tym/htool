package com.example

import scala.swing._
import scala.swing.Dialog.Result
import javax.swing._
import java.awt.Font
import java.awt.event._
import javax.swing.event._
import java.util.Date

class fusenCard( px : Int, py : Int, w : Int, h : Int, var date : Date, var message : String, upd : fusenCard => Unit ) extends JFrame {

	var delete = false

	setLayout( null )

	setUndecorated( true )
	setOpacity( 0.7f )
	setVisible( true )

	setBounds( px, py, w, h )

	var lsn = new mouseListener( this )

	var dateText = new JTextArea()
	dateText.setFocusable( true )
	dateText.setEditable( false )
	dateText.setLineWrap( false )
	dateText.setBounds( 0, 0, w, 20 )
	dateText.setText( "%tY/%<tm/%<td".format( date ) )
	add( dateText )

	var txtLsn = new textListener( this )
	var txtLsn2 = new textListener2( this )

	var text = new JTextArea()
	text.setFocusable( true )
	text.setEditable( true )
	text.setLineWrap( true )
	text.setBounds( 0, 20, w, h - 20 )
	text.setText( message )
	text.addKeyListener( txtLsn )
	text.addFocusListener( txtLsn2 )
	text.addMouseListener( lsn )

	add( text )


	println( "付箋を作成 pos=(" + px + "," + py + ") siz=(" + w + "," + h + ")" )

	def setDelete = {
		delete = true
		text.setEditable( false )
		text.removeMouseListener( lsn )

		var remove = new JLabel( "Remove" )
		remove.setFont( new Font( "Arial", Font.BOLD, 48 ) )
		remove.setBounds( 0, 0, w, h )
		remove.setOpaque( false )
		remove.setVisible( true )
		add( remove )

		dateText.setVisible( false )
		text.setVisible( false )

		text.setText( "deleted" )
		println( "削除しました。" )
	}

	def changeText = {

		upd( this )
		println( "textが編集されました。" )

	}

}

class mouseListener( cls : fusenCard ) extends MouseAdapter {

	override def mousePressed( me : MouseEvent ) {

		// if ( javax.swing.SwingUtilities.isRightMouseButton( me ) ) println( "右クリック" )

	}

	override def mouseClicked( me : MouseEvent ) {

		if ( javax.swing.SwingUtilities.isRightMouseButton( me ) ) {

			println( "右クリック" )

			Dialog.showConfirmation( title="確認", message="削除しますか？" ) match {
				case Result.Yes => cls.setDelete
				case _          =>
			}

		}

	}

	override def mouseDragged( me : MouseEvent ) {
	}

}

class textListener( cls : fusenCard ) extends KeyListener {

	override def keyTyped( e : KeyEvent ) = {
	}

	override def keyPressed( e : KeyEvent ) = {
	}

	override def keyReleased( e : KeyEvent ) = {
	}

}

class textListener2( cls : fusenCard ) extends FocusListener {

	def focusGained( e : FocusEvent ) = {
	}

	def focusLost( e : FocusEvent ) = {

		cls.changeText

	}

}



