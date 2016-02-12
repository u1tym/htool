

class Coor( str : String ) {

	var hour   : Int =    _
	var minute : Int =    _
	var second : Int =    _
	var milli  : Int =    _
	var dir    : String = _

	dir = getDir( str )
	require( expireDir( dir ) )

	hour   = getHour( str, dir )
	require( expireHour( hour, dir ) )

	minute = getMinute( str, dir )
	require( expireMinute( hour, minute, dir ) )

	second = getSecond( str, dir )
	require( expireSecond( hour, second, dir ) )

	milli  = getMilli( str, dir )
	require( expireMilli( hour, milli, dir ) )


	def getHour( s : String, d : String ) : Int = {
		d match {
			case "N" | "S" => s.substring( 0, 2 ).toInt
			case "E" | "W" => s.substring( 0, 3 ).toInt
		}
	}

	def getMinute( s : String, d : String ) : Int = {
		d match {
			case "N" | "S" => s.substring( 2, 4 ).toInt
			case "E" | "W" => s.substring( 3, 5 ).toInt
		}
	}

	def getSecond( s : String, d : String ) : Int = {
		d match {
			case "N" | "S" => s.substring( 4, 6 ).toInt
			case "E" | "W" => s.substring( 5, 7 ).toInt
		}
	}

	def getMilli( s : String, d : String ) : Int = {
		d match {
			case "N" | "S" => s.substring( 6, 9 ).toInt
			case "E" | "W" => s.substring( 7, 10 ).toInt
		}
	}

	def getDir( s : String ) = s.substring( s.length - 1, s.length )

	def expireDir( d : String ) : Boolean = {
		d match {
			case "N" | "E" | "W" | "S" => true
			case _                     => false
		}
	}

	def expireHour( h : Int, d : String ) = {
		d match {
			case "N" | "S" => if ( h >= 0 && h <= 90 ) true else false
			case "E" | "W" => if ( h >= 0 && h <= 180 ) true else false
			case _         => false
		}
	}

	def expireMinute( h : Int, m : Int, d : String ) = {
		( d, h ) match {
			case ( "N" | "S", 90 )  => if ( m == 0 ) true else false
			case ( "E" | "W", 180 ) => if ( m == 0 ) true else false
			case _                  => if ( m >= 0 && m <= 59 ) true else false
		}
	}

	def expireSecond( h : Int, s : Int, d : String ) = {
		( d, h ) match {
			case ( "N" | "S", 90 )  => if ( s == 0 ) true else false
			case ( "E" | "W", 180 ) => if ( s == 0 ) true else false
			case _                  => if ( s >= 0 && s <= 59 ) true else false
		}
	}

	def expireMilli( h : Int, t : Int, d : String ) = {
		( d, h ) match {
			case ( "N" | "S", 90 )  => if ( t == 0 ) true else false
			case ( "E" | "W", 180 ) => if ( t == 0 ) true else false
			case _                  => if ( t >= 0 && t <= 999 ) true else false
		}
	}

	def debugPrint = {

		println( "H = [" + hour + "]" )
		println( "M = [" + minute + "]" )
		println( "S = [" + second + "]" )
		println( "T = [" + milli + "]" )
		println( "X = [" + dir + "]" )

	}

}


