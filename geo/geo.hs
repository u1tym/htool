module Main where

import System.Environment( getArgs )

-- 緯度経度
-- 単位は度（0〜360）
data GeoPoint = GeoPoint {
	latitude  :: Double,
	longitude :: Double
} deriving( Show )

-- 3次元空間上の点
data Point3D = Point3D {
	x :: Double,
	y :: Double,
	z :: Double
} deriving( Show )

-- 3次元空間上のベクトル
data Vector3D = Vector3D {
	vx :: Double,
	vy :: Double,
	vz :: Double
} deriving( Show )


-- 3次元空間上の直線
-- 通過点Pと方向ベクトルVを保持する。
data Line = Line {
	point  :: Point3D,		-- 通過点P
	vector :: Vector3D		-- 方向ベクトルV
}

-- 3次元空間上の平面
-- 以下の方程式を満たす係数 a, b, c, d を保持する。
-- F : ax + by + cz + d = 0
data Field = Field {
	la :: Double,			-- 係数a
	lb :: Double,			-- 係数b
	lc :: Double,			-- 係数c
	ld :: Double			-- 係数d
}

-- 緯度経度から3次元空間上の点の座標を取得する。
-- 3次元空間上の座標の単位はm。
-- 軸の向きは以下の通り。
--  0N 0E -> ( max,   0,   0 )
--  0N90E -> (   0, max,   0 )
-- 90N 0E -> (   0,   0, max )
convGeoTo3d :: GeoPoint -> Point3D
convGeoTo3d src = Point3D{ x = value_x, y = value_y, z = value_z }
	where --
	      phi    = convDegreeToRadian $ latitude  src

	      --
	      lambda = convDegreeToRadian $ longitude src

	      --
	      a      = 6378137.0

	      --
	      f      = 1.0 / 298.257223563

	      -- e^2 = 2 * f - f^2
	      e2     = 2.0 * f - f * f

	      -- N = a / sqrt( 1 - e^2 * sin^2 )
	      n      = a / ( sqrt ( 1.0 - e2 * sin phi * sin phi ) )

	      value_x = convFloor $ n * cos phi * cos lambda
	      value_y = convFloor $ n * cos phi * sin lambda
	      value_z = convFloor $ n * ( 1 - e2 ) * sin phi


-- 3次元空間上の座標から緯度経度を取得する。
conv3dToGeo :: Point3D -> GeoPoint
conv3dToGeo src = GeoPoint{ latitude = lat, longitude = lon }
	where px = x src
	      py = y src
	      pz = z src

	      -- 
	      f      = 1.0 / 298.257223563

	      -- 
	      a      = 6378137.0

	      -- 
	      b      = a * ( 1.0 - f )

	      -- 
	      e      = sqrt( a * a - b * b ) / a

	      lambda = conv3dToGeo_lambda px py
	      phi    = conv3dToGeo_phi a b px py pz

	      lat = convFloor $ convRadianToDegree phi
	      lon = convFloor $ convRadianToDegree lambda

-- 3次元空間上の地点座標から経度を取得する。
conv3dToGeo_lambda :: Double -> Double -> Double
conv3dToGeo_lambda x y
  | x == 0.0 && y >= 0.0 =  3.14159265 / 2.0
  | x == 0.0 && y <  0.0 = -3.14159265 / 2.0
  | x <  0.0 && y >= 0.0 = atan( y / x ) + 3.14159265
  | x <  0.0 && y <  0.0 = atan( y / x ) - 3.14159265
  | otherwise            = atan( y / x )

-- 3次元空間上の地点座標から緯度を取得する。
conv3dToGeo_phi :: Double -> Double -> Double -> Double -> Double -> Double
conv3dToGeo_phi a b x y z = phi
	where t   = a * a * z / ( b * b * sqrt( x * x + y + y ) )
	      phi = atan( t )

-- 度表記からラジアン表記への変換
convDegreeToRadian :: Double -> Double
convDegreeToRadian src = src * 3.14159265 / 180.0

-- ラジアン表記から度表記への変換
convRadianToDegree :: Double -> Double
convRadianToDegree src = src * 180.0 / 3.14159265

-- 小数点以下1桁への切り捨て
convFloor :: Double -> Double
convFloor src = val
	where num = ( truncate $ src * 10.0 )
	      val = ( fromIntegral num :: Double ) / 10.0

main = do

	args <- getArgs

	let com = if length args == 0 then "" else args !! 0

	putStr $ case com of
				"geo_to_xyz" -> calc_geo_to_xyz $ tail args
				_            -> usage

	print $ convGeoTo3d GeoPoint{ latitude  =  0.0, longitude =  0.0 }
	print $ convGeoTo3d GeoPoint{ latitude  =  0.0, longitude = 90.0 }
	print $ convGeoTo3d GeoPoint{ latitude  = 90.0, longitude =  0.0 }


usage :: String
usage = "usage\n"
	    ++ "  geo_to_xyz latitde longitude\n"

calc_geo_to_xyz :: [ a ] -> String
calc_geo_to_xyz args
	| length args < 2 = usage
	| otherwise       = "aa"

conv_LatitudeToDouble :: String -> Double
conv_LatitudeToDouble src
	| length src < 7  = read src :: Double
	| src !! 6 == 'N' = conv_LatitudeToDouble_Core src
	| src !! 6 == 'S' = conv_LatitudeToDouble_Core src * ( -1.0 )
	| otherwise       = read src :: Double

conv_LatitudeToDouble_Core :: String -> Double
conv_LatitudeToDouble_Core src = h + ( m / 60.0 ) + ( s / 3600.0 )
	where
		h = read [ src !! 0 , src !! 1 ] :: Double
		m = read [ src !! 2 , src !! 3 ] :: Double
		s = read [ src !! 4 , src !! 5 ] :: Double

