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

	def *( d : Direction ) : Coordinate = vincenty( d )

	def vincenty( d : Direction ) : Coordinate = {

		val ra : Double = 6378137.0				// 赤道半径(m)
		val rb : Double = 6356752.0				// 極半径(m)
		val f  : Double = ( ra - rb ) / ra		// 扁平率

		val lat : Double = latitude.toInt  * PI / 1000.0 / 3600.0 / 180.0
		val lon : Double = longitude.toInt * PI / 1000.0 / 3600.0 / 180.0

		val alpha12 : Double = d.bearing.radian
		val s       : Double = d.distance.meter

		val U1     : Double = atan( ( 1 - f ) * tan( lat ) )
		val sigma1 : Double = atan( tan( U1 ) / cos( alpha12 ) )
		val alpha  : Double = asin( cos( U1 ) * sin( alpha12 ) )
		val u2     : Double = pow( cos( alpha ), 2.0 ) * ( pow( ra, 2 ) - pow( rb, 2 ) ) / pow( rb, 2 )
		// println( "U1 = " + U1 )
		// println( "σ1 = " + sigma1 )
		// println( "α  = " + alpha )
		// println( "u2 = " + u2 )

		val VA : Double = 1.0 + u2 / 16384.0 * ( 4096.0 + u2 * ( -768.0 + u2 * ( 320.0 - 175.0 * u2 ) ) )
		val VB : Double = u2 / 1024.0 * ( 256.0 + u2 * ( -128.0 + u2 * ( 74.0 - 47.0 * u2 ) ) )
		// println( "VA = " + VA )
		// println( "VB = " + VB )

		var sigma   : Double = s / ( rb * VA )
		// println( "σ  = " + sigma )
		var sigma0  : Double = 0.0
		var sigma2m : Double = 0.0
		do {
			sigma0  = sigma
			sigma2m = 2.0 * sigma1 + sigma
			var tmp1 : Double = cos( sigma ) *
								( -1.0 + 2.0 * pow( cos( sigma2m ), 2.0 ) ) -
								VB / 6.0 * cos( sigma2m ) *
								( -3.0 + 4.0 * pow( sin( sigma2m ), 2.0 ) ) *
								( -3.0 + 4.0 * pow( cos( sigma2m ), 2.0 ) )
			var delta_sigma : Double = VB * sin( sigma ) * cos( sigma2m + VB / 4.0 * tmp1 )

			sigma = s / ( rb * VA ) + delta_sigma

		} while ( abs( sigma0 - sigma ) > 0.000000001 )

		// println( "result" )
		// println( "σ  = " + sigma )
		// println( "2σ = " + sigma2m )

		val tan_psi2 : Double = ( sin( U1 ) * cos( sigma ) +
									cos( U1 ) * sin( sigma ) * cos( alpha12 ) ) /
							 	( ( 1 - f ) * sqrt( pow( sin( alpha ), 2.0 ) +
									pow( sin( U1 ) * sin( sigma ) - cos( U1 ) * cos( sigma ) * cos( alpha12 ), 2.0 ) ) )
		val psi2     : Double = atan( tan_psi2 )
		// println( "tan ψ2 = " + tan_psi2 )
		// println( "ψ2     = " + psi2 )

		val omega        : Double = sin( sigma ) * sin( alpha12 ) /
									( cos( U1 ) * cos( sigma ) -
									sin( U1 ) * sin( sigma ) * sin( alpha12 ) )
		val C            : Double = f / 16.0 * pow( cos( alpha ), 2.0 ) *
									( 4.0 + f * ( 4.0 - 3.0 * pow( cos( alpha ), 2.0 ) ) )
		val tmp2         : Double = cos( sigma2m ) + C * cos( sigma ) *
									( -1.0 + 2.0 * pow( cos( sigma2m ), 2.0 ) )
		val delta_lambda : Double = omega - ( 1.0 - C ) * f * sin( alpha ) *
									( sigma + C * sin( alpha ) * tmp2 )
		val lambda2      : Double = lon + delta_lambda
		// println( "ω      = " + omega )
		// println( "C      = " + C )
		// println( "Δλ     = " + delta_lambda )
		// println( "λ2     = " + lambda2 )

		var new_latitude_int  = ( psi2    * 180.0 / PI * ( 3600000.0 ) ).toInt
		var new_longitude_int = ( lambda2 * 180.0 / PI * ( 3600000.0 ) ).toInt

		var new_latitude  = new DMST( new_latitude_int )
		var new_longitude = new DMST( new_longitude_int )

		var new_coordinate = new Coordinate( new_latitude, new_longitude )

		new_coordinate
	}

	def value : String = "%02d%02d%02d%03d%s%03d%02d%02d%03d%s".format(
				latitude.degree,
				latitude.minute,
				latitude.second,
				latitude.milliS,
				if ( latitude.sign >= 0 ) "N" else "S",
				longitude.degree,
				longitude.minute,
				longitude.second,
				longitude.milliS,
				if ( longitude.sign >= 0 ) "E" else "W" )

	def print = println( value )
}

/**
 * 方位（真方位）
 */
class Bearing( v : Double ) {

	var degree : Double = ( v % 360.0 + 360.0 ) % 360.0		// 度（0.0〜360.0）

	/**
	 * ラジアン値
	 */
	// def radian : Double = degree * PI / 180.0
	def radian : Double = if ( normalization > 180 ) ( normalization - 360.0 ) * PI / 18.0 else normalization * PI / 18.0

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
object DistanceUnit extends Enumeration {
  type DistanceUnit = Value

  val  nm, meter = Value
}
import DistanceUnit._

object Distance {
    val unit_nm    = DistanceUnit.nm
    val unit_meter = DistanceUnit.meter
}

class Distance( v : Double ) {

	var meter : Double = v

	require( meter >= 0 )

	def this( v : Int ) = this( v.toDouble )

	def nm : Double = meter / 1852.0

	def setNm( v : Double ) = {
		meter = v / 1852.0
	}

	def update( typ : DistanceUnit, v : Double ) = {

		typ match {
			case Distance.unit_meter => println( "meter" )
						       meter = v
			case Distance.unit_nm    => println( "nm" )
							   setNm( v )
			case _ => println( "error" )
		}

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

	def this( degree : Double, meter : Double ) = {
		this()

		bearing  = new Bearing( degree )
		distance = new Distance( meter )
	}

	def +( v : Distance ) : Direction = {
		new Direction( bearing, ( distance + v ) )
	}

	def -( v : Distance ) = {
		new Direction( bearing, ( distance - v ) )
	}

	def +( v : Bearing ) = {
		new Direction( bearing + v, distance )
	}

	def -( v : Bearing ) = {
		new Direction( bearing - v, distance )
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

	/**
	 * DMST( d, m, s, t, n )
	 *
	 * 地点座標の生成
	 *
	 * @param  d  度    （ -180～ 180）
	 * @param  m  分    （  -59～  59）
	 * @param  s  秒    （  -59～  59）
	 * @param  t  ミリ秒（-1000～1000）
	 * @param  n  方位  （ N, E, W, S ）
	 *
	 * @return インスタンス
	 */
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

	/**
	 * DMST( str )
	 *
	 * 地点座標の生成
	 *
	 * @param  str 地点の文字列
	 *             DD[MM[SS[ttt]]](N|S)
	 *             DDD[MM[SS[ttt]]](E|W)
	 *
	 * @return インスタンス
	 */
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

	/**
	 * isLatitude
	 *
	 * 緯度値妥当性チェック
	 *
	 * @param   なし
	 *
	 * @return  true  妥当
	 * @return  false 妥当ではない
	 */
	def isLatitude : Boolean = {
		if ( toInt( degree, minute, second, milliS ) <= toInt( 90, 0, 0, 0 ) ) true else false
	}

	/**
	 * isLongitude
	 *
	 * 経度値妥当性チェック
	 *
	 * @param   なし
	 *
	 * @return  true  妥当
	 * @return  false 妥当ではない
	 */
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
		println( "D = [" + degree + "]" )
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


