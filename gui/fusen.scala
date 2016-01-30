package com.example

import java.util.Date
import java.io.PrintWriter

class fusen( displayW : Int, displayH : Int, cardW : Int, cardH : Int ) {

	var data : Map[ Int, fusenCard ] = Map.empty[ Int, fusenCard ]
	var number    : Int = 0
	var cardCount : Int = 0

	def makeCard( date : Date, message : String ) = {

		val id       = numbering

		val func     = write _
		val upd      = func( id, _ : fusenCard )

		val ( x, y ) = position( cardCount )
		val card     = new fusenCard( x, y, cardW, cardH, date, message, upd )

		cardCount    += 1

		data =  data + ( id -> card )

		write( id, card )

	}

	def numbering = {
		number += 1
		number
	}


    def position( i : Int ) : ( Int, Int ) = {
        val hcm = ( displayH / cardH ).toInt
        println( "hcm=" + hcm )

        val rx = displayW - ( cardW * ( ( cardCount / hcm ).toInt + 1 ) )
        val ry = displayH - cardH * ( cardCount % hcm + 1 )

        ( rx, ry )
    }

	def write( id : Int, card : fusenCard ) : Unit = {

		println( "更新しました。" )

		val filename = "data_%04d.dat".format( id )

		val file = new PrintWriter( filename )

		file.write( "%d\n".format( id ) )
		file.write( "%tY/%<tm/%<td\n".format( card.date ) )
		file.write( "%s\n".format( card.message ) )

		file.close()

		return ()
	}

}

