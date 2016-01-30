package com.example

import scala.swing._
import scala.swing.event._
import java.util.Date

object Main extends SimpleSwingApplication{

	val btnSample = new Button {
						text = "sample"
						reactions += {
							case e : ButtonClicked => {
								fStock.makeCard( new Date, "abcde" )
							}
						}
					}

	val d = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode()

	// 画面サイズ取得
	val w = d.getWidth()
	val h = d.getHeight()
	println( "display=" + w + "x" + h )

	// 付箋情報
	val cardW = 300
	val cardH = 100

	var fStock = new fusen( w, h, cardW, cardH )

	def top = new MainFrame {

		title    = "BOOOOM"
		contents = new FlowPanel {
			preferredSize  = new Dimension( 200, 100 )
			contents      += btnSample
		}

	}

}

