import java.lang.Math._

/**
 * 座標（緯度経度）
 */
class Coordinate {
	var latitude  : DMST = _		// 緯度
	var longitude : DMST = _		// 経度

	latitude  = new DMST( "00N" )
	longitude = new DMST( "000E" )

	def this( str : String ) {

		this()

		// 緯度抽出
		val tmp = "^[0-9].([0-9].([0-9].([0-9]..){0,1}){0,1}){0,1}(N|S)".r.findFirstIn( str ).getOrElse( "9999999999X" )

		latitude  = new DMST( tmp )
		longitude = new DMST( str.substring( tmp.length, str.length ) )

	}

	def this( lat : DMST, lon : DMST ) {

		this()

		require( lat.isLatitude )
		require( lon.isLongitude )

		latitude  = lat
		longitude = lon

	}

	def *( d : Direction ) : Coordinate = {

		val rx : Double = 6378137.0		// 赤道半径(m)
		val e2 : Double = 0.00669437999	// 離心率(e^2)

		var wt  = sqrt( 1.0 - e2 * pow( sin( latitude.toInt / 1000 * PI / 180.0 ), 2.0 ) )
		var mt  = rx * ( 1.0 - e2 ) / pow( wt, 3.0 )
		var dit = d.distance.meter * cos( d.bearing.radian ) / mt

		var i   = latitude.toInt / 1000 * PI / 180.0 + dit / 2.0

		var w   = sqrt( 1.0 - e2 * pow( sin( i ), 2.0 ) )
		var m   = rx * ( 1.0 - e2 ) / pow( w, 3.0 )
		var n   = rx / w

		var di  = d.distance.meter * cos( d.bearing.radian ) / m
		var dk  = d.distance.meter * sin( d.bearing.radian ) / ( n * cos( i ) )

		var new_latitude_int  = ( latitude.toInt  + di * 180.0 / PI * 1000.0 ).toInt
		var new_longitude_int = ( longitude.toInt + dk * 180.0 / PI * 1000.0 ).toInt

		var new_latitude  = new DMST( new_latitude_int )
		var new_longitude = new DMST( new_longitude_int )

		var new_coordinate = new Coordinate( new_latitude, new_longitude )

		new_coordinate
	}

	def print = {
		println( "緯度" )
		latitude.print
		println( "経度" )
		longitude.print
	}

}

/**
 * 方位（真方位）
 */
class Bearing( v : Double ) {

	var degree : Double = ( v % 360.0 + 360.0 ) % 360.0		// 度（0.0〜360.0）

	/**
	 * ラジアン値
	 */
	def radian : Double = degree * 3.14159265 / 180.0

	/**
	 * 正規化
	 */
	def normalization : Double = {
		degree = ( degree % 360.0 + 360.0 ) % 360.0
		degree
	}

	def +( v : Bearing ) : Bearing = {
		new Bearing( degree + v.degree )
	}

	def -( v : Bearing ) : Bearing = {
		new Bearing( degree - v.degree )
	}

	def *( v : Double ) : Bearing = {
		new Bearing( degree * v )
	}

	def *( v : Int ) : Bearing = {
		this .* ( v.toDouble )
	}

	def /( v : Double ) : Bearing = {
		new Bearing( degree / v )
	}

	def /( v : Int ) : Bearing = {
		this ./ ( v.toDouble )
	}

	def /( v : Bearing ) : Double = degree / v.degree

}

/**
 * 距離
 */
class Distance( v : Double ) {

	var meter : Double = v

	require( meter >= 0 )

	def this( v : Int ) = this( v.toDouble )

	def nm : Double = meter / 1852.0

	def setNm( v : Double ) = {
		meter = v * 1852.0
	}

	def +( v : Distance ) : Distance = {
		new Distance( meter + v.meter )
	}

	def +( v : Double ) : Distance = {
		this .+ ( new Distance( v ) )
	}

	def +( v : Int ) : Distance = {
		this .+ ( new Distance( v ) )
	}

	def -( v : Distance ) : Distance = {
		new Distance( if ( meter - v.meter >= 0 ) meter - v.meter else 0.0 )
	}

	def -( v : Double ) : Distance = {
		this .- ( new Distance( v ) )
	}

	def -( v : Int ) : Distance = {
		this .- ( new Distance( v ) )
	}

	def *( v : Double ) : Distance = {
		new Distance( if ( v >= 0.0 ) meter * v else 0.0 )
	}

	def *( v : Int ) : Distance = {
		this .* ( v.toDouble )
	}

	def /( v : Double ) : Distance = {
		new Distance( if ( v >= 0.0 ) meter / v else 0.0 )
	}

	def /( v : Int ) : Distance = {
		this ./ ( v.toDouble )
	}

	def /( d : Distance ) : Double = meter / d.meter

	def print = {
		println( "distance = " + meter + " meter" )
	}
}

class Direction {

	var bearing  : Bearing  = new Bearing( 0 )
	var distance : Distance = new Distance( 0 )

	def this( b : Bearing, d : Distance ) {
		this()

		bearing  = b
		distance = d
	}

	def this( angl : Double, dist : Double ) = {
		this()

		bearing  = new Bearing( angl )
		distance = new Distance( dist )
	}

}

class DMST {

	var degree : Int = _
	var minute : Int = _
	var second : Int = _
	var milliS : Int = _
	var sign   : Int = 1

	def this( v : Int ) {

		this()

		var base = if ( v >= 0 ) v else ( v * -1 )

		var d  = base / toInt( 1, 0, 0, 0 )
		var m  = ( base - toInt( d, 0, 0, 0 ) ) / toInt( 0, 1, 0, 0 )
		var s  = ( base - toInt( d, m, 0, 0 ) ) / toInt( 0, 0, 1, 0 )
		var t  = ( base - toInt( d, m, s, 0 ) ) / toInt( 0, 0, 0, 1 )
		var sg = v / base

		create( d, m, s, t, sg )

	}

	def this( d : Int, m : Int, s : Int, t : Int, n : String ) {

		this()

		// 値チェック(方位)
		require( n match { case "N" | "E" | "W" | "S" => true
						   case _                     => false } )

		create( d, m, s, t, if ( n == "N" || n == "E" ) 1 else -1 )

		// 値チェック(上限)
		require( toInt( d, m, s, t ) <= ( n match { case "N" | "S" => toInt(  90, 0, 0, 0 )
												    case "E" | "W" => toInt( 180, 0, 0, 0 ) } ) )
	}

	def this( str : String ) {

		this()

		// 方位取得
		var n = getDir( str )

		// 度、分、秒、ミリ秒取得
		var d = getDegree( str, n )
		var m = getMinute( str, n )
		var s = getSecond( str, n )
		var t = getMilliS( str, n )

		// 値チェック(方位)
		require( n match { case "N" | "E" | "W" | "S" => true
						   case _                     => false } )

		create( d, m, s, t, if ( n == "N" || n == "E" ) 1 else -1 )

		// 値チェック(上限)
		require( toInt( d, m, s, t ) <= ( n match { case "N" | "S" => toInt(  90, 0, 0, 0 )
												    case "E" | "W" => toInt( 180, 0, 0, 0 ) } ) )

	}

	private def create( d : Int, m : Int, s : Int, t : Int, sg : Int ) = {

		degree = d
		minute = m
		second = s
		milliS = t
		sign   = sg

		// 値チェック(方位)
		require( sign == 1 || sign == -1 )

		// 値チェック(度)
		require( degree >= 0 && degree <= 180 )

		// 値チェック(分)
		require( minute >= 0 && minute <=  59 )

		// 値チェック(秒)
		require( second >= 0 && second <=  59 )

		// 値チェック(ミリ秒)
		require( milliS >= 0 && milliS <= 999 )

	}

	def isLatitude : Boolean = {
		if ( toInt( degree, minute, second, milliS ) <= toInt( 90, 0, 0, 0 ) ) true else false
	}

	def isLongitude : Boolean = {
		if ( toInt( degree, minute, second, milliS ) <= toInt( 180, 0, 0, 0 ) ) true else false
	}

	/**
	 * toInt
	 *
	 * 単位をミリ秒とした整数値を返す。
	 *
	 * @param   なし
	 *
	 * @return  単位をミリ秒とした整数値(-648000000〜648000000)
	 */
	def toInt : Int = {
		toInt( degree, minute, second, milliS ) * sign
	}

	/**
	 * print
	 *
	 * デバッグプリントを出力する。
	 *
	 * @param   なし
	 *
	 * @return  なし
	 */
	def print = {
		println( "H = [" + degree + "]" )
		println( "M = [" + minute + "]" )
		println( "S = [" + second + "]" )
		println( "T = [" + milliS + "]" )
		println( "X = [" + sign   + "]" )
	}

	/**
	 * toInt( degree, minute, second, milliS ) : Int
	 *
	 * 単位をミリ秒とした整数値を返す。符号は常に正とする。
	 *
	 * @param   degree : 度(0〜180)
	 * @param   minute : 分(0〜59)
	 * @param   second : 秒(0〜59)
	 * @param   milliS : ミリ秒(0〜999)
	 *
	 * @return  単位をミリ秒とした整数値(0〜648000000)
	 */
	private def toInt( d : Int, m : Int, s : Int, t : Int ) : Int = {
		( ( d * 60 + m ) * 60 + s ) * 1000 + t
	}

	private def getDir( s : String ) = s.substring( s.length - 1, s.length )

	private def getDegree( s : String, d : String ) : Int = {
		d match { case "N" | "S" => s.substring( 0, 2 ).toInt
				  case "E" | "W" => s.substring( 0, 3 ).toInt }
	}

	private def getMinute( s : String, d : String ) : Int = {
		d match { case "N" | "S" => if ( s.length < 4 ) 0 else s.substring( 2, 4 ).toInt
				  case "E" | "W" => if ( s.length < 5 ) 0 else s.substring( 3, 5 ).toInt }
	}

	private def getSecond( s : String, d : String ) : Int = {
		d match { case "N" | "S" => if ( s.length < 6 ) 0 else s.substring( 4, 6 ).toInt
				  case "E" | "W" => if ( s.length < 7 ) 0 else s.substring( 5, 7 ).toInt }
	}

	private def getMilliS( s : String, d : String ) : Int = {
		d match { case "N" | "S" => if ( s.length <  9 ) 0 else s.substring( 6, 9 ).toInt
				  case "E" | "W" => if ( s.length < 10 ) 0 else s.substring( 7, 10 ).toInt }
	}
}


