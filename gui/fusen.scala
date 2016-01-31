package utl.plan

import java.util.Date
import java.io.PrintWriter
import java.io._
import java.util.Timer
import java.util.TimerTask
import scala.io.Source

class fusen( displayW : Int, displayH : Int, cardW : Int, cardH : Int ) {

	var data : Map[ Int, fusenCard ] = Map.empty[ Int, fusenCard ]
	var number    : Int = 0
	var cardCount : Int = 0

	def makeCard( message : String ) = {

		// ID採番
		val id       = numbering

		// 更新関数
		val func     = write _
		val upd      = func( id, _ : fusenCard )

		// 位置
		val ( x, y ) = position( cardCount )

		// 期限（現在日から7日後）
		val date = new Date( new Date().getTime + 7 * 24 * 60 * 60 * 1000 )

		// カード生成
		val card     = new fusenCard( x, y, cardW, cardH, date, message, upd )

		cardCount    += 1

		data =  data + ( id -> card )

		write( id, card )

	}

	def recoveryCard = {

		new File( "./" ).listFiles.flatMap {

			case f if ( f.isFile && f.getName.split( "\\." ).last == "dat" ) => {
				println( "target [" + f.getName + "]" )
				read( f.getName )
				List()
			}
			case _ => List()

		}
	}

	def setTimer = {

		var timer = new Timer
		timer.schedule( new doTimer( this ), 0, 1000 )

	}

	def doEvent = {

		println( "タイマー発動" )

	}

	def numbering = {
		
		var number = 1
		while( data.getOrElse( number, "empty" ) != "empty" ) {
			println( "no=" + number + "はすでに存在" )
			number += 1
		}

		number
	}


    def position( i : Int ) : ( Int, Int ) = {
        val hcm = ( displayH / cardH ).toInt
        println( "hcm=" + hcm )

        val rx = displayW - ( cardW * ( ( cardCount / hcm ).toInt + 1 ) )
        val ry = displayH - cardH * ( cardCount % hcm + 1 )

        ( rx, ry )
    }

	def read( filename : String ) : Unit = {

		val file = Source.fromFile( filename )

		val lines = file.getLines

		val id        = lines.next.toInt
		val strDate   = lines.next
		val strStatus = lines.next
		var strMessage : String = ""
		while( lines.hasNext ) {
			strMessage += lines.next
			strMessage += "\n"
		}

		if ( strStatus == "remove" ) {
			new File( filename ).delete
			return ()
		}

		val yy = strDate.split( "/" )(0).toInt - 1900
		val mm = strDate.split( "/" )(1).toInt - 1
		val dd = strDate.split( "/" )(2).toInt

		val date = new Date( yy, mm, dd )

		println( "id = [" + id + "]" )
		println( "dt = [" + date + "]" )
		println( "ms = [" + strMessage + "]" )


		val func     = write _
		val upd      = func( id, _ : fusenCard )

		val ( x, y ) = position( cardCount )
		val card     = new fusenCard( x, y, cardW, cardH, date, strMessage, upd )

		cardCount    += 1

		data =  data + ( id -> card )

	}

	def write( id : Int, card : fusenCard ) : Unit = {

		println( "更新しました。" )

		val filename = "data_%04d.dat".format( id )

		val file = new PrintWriter( filename )

		file.write( "%d\n".format( id ) )
		file.write( "%tY/%<tm/%<td\n".format( card.date ) )
		file.write( if ( card.delete ) "remove\n" else "live\n" )
		file.write( "%s\n".format( card.message ) )

		file.close()

		return ()
	}

}

class doTimer( cls : fusen ) extends TimerTask {

	def run = {

		cls.doEvent

	}

}

