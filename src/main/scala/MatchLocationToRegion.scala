import scala.io.Source
import java.io.PrintWriter
import play.api.libs.json._
import play.api.libs.functional.syntax._

//Classes for coordinates, location, region, and results
case class Coordinates(x: Double, y: Double)
case class Polygon(coordinates: List[Coordinates])
case class Region(name: String, polygons: List[Polygon])
case class Location(name: String, coordinates: List[Coordinates])
case class Results(region: String, matchedLocations: List[String])

object MatchLocationToRegion {
    def main(args: Array[String]): Unit = {
        val locationFile = args(0)
        val regionFile = args(1)
        val outputFile = args(2)

        //Read JSON data from the locations and regions files
        val locationsJson = Source.fromFile(locationFile).mkString
        val regionsJson = Source.fromFile(regionFile).mkString

        //Parse JSON data from the locations and regions files
        val locations = parseLocations(locationsJson)
        val regions = parseRegions(regionsJson)

        //Save results and store them in JSON
        val results = matchLocationsToRegions(locations, regions)
        val resultsJson = resultsToJson(results)

        //Write to the results file
        val writer = new PrintWriter(outputFile)
        writer.println(resultsJson)
        writer.close()
    }

    // Parse locations JSON containing data and return a list of 'Location' objects
    def parseLocations(jsonString: String): List[Location] = {
        val json = Json.parse(jsonString)
        val locationList = json.validate[List[JsObject]]

        locationList.fold(
        errors => {
            println("Failed to parse locations")
            errors.foreach(println)
            List.empty[Location]
        },
        values => {
            values.flatMap { location =>
                val name = (location \ "name").validate[String]
                val coordinates = (location \ "coordinates").validate[List[Double]]

                (name, coordinates) match {
                    case (JsSuccess(n, _), JsSuccess(coords, _)) =>
                    Some(Location(n, coordsToCoordinates(coords)))
                    case _ => None
                }
            }
        }
        )
    }

    // Convert list of doubles to a list of coordinates
    def coordsToCoordinates(coords: List[Double]): List[Coordinates] = {
        coords.sliding(2, 2).collect {
        case List(x, y) => Coordinates(x, y)
        }.toList
    }

    // Parse regions JSON containing data and return a list of 'Region' objects
    def parseRegions(jsonString: String): List[Region] = {
        val json = Json.parse(jsonString)
        val regionList = json.validate[List[JsObject]]

        regionList.fold(
        errors => {
            println("Failed to parse regions")
            errors.foreach(println)
            List.empty[Region]
        },
        values => {
            values.flatMap { region =>
                val name = (region \ "name").validate[String]
                val coordinates = (region \ "coordinates").validate[List[List[List[Double]]]]

                (name, coordinates) match {
                    case (JsSuccess(n, _), JsSuccess(coords, _)) =>
                    Some(Region(n, parsePolygons(coords)))
                    case _ => None
                }
            }
        }
        )
    }

    // Parse list of polygons and return a list of 'Polygon' objects
    def parsePolygons(polygons: List[List[List[Double]]]): List[Polygon] = {
        polygons.flatMap { polygon =>
        val coordinates = polygon.flatMap(coordsToCoordinates)
        if (coordinates.nonEmpty)
            Some(Polygon(coordinates))
        else
            None
        }
    }

    // Match locations to regions and return a list of 'Results' objects
    def matchLocationsToRegions(locations: List[Location], regions: List[Region]): List[Results] = {
        regions.map { region =>
            val matchedLocations = locations.filter(location => isLocationInRegion(location, region))
            Results(region.name, matchedLocations.map(_.name))
        }
    }

    // Check if a location is inside a region
    def isLocationInRegion(location: Location, region: Region): Boolean = {
        val point = location.coordinates
        val polygons = region.polygons
        polygons.exists(isPointInPolygon(point, _))
    }

    // Check if a point is inside a polygon
    def isPointInPolygon(point: List[Coordinates], polygon: Polygon): Boolean = {
        val x = point.head.x
        val y = point.head.y
        val coordinates = polygon.coordinates
        val n = coordinates.length
        var inside = false
        var p1x = coordinates(0).x
        var p1y = coordinates(0).y

        for (i <- 0 until n) {
        val p2x = coordinates((i + 1) % n).x
        val p2y = coordinates((i + 1) % n).y

        if (((p1y <= y && y < p2y) || (p2y <= y && y < p1y)) &&
            x < ((p2x - p1x) / (p2y - p1y) * (y - p1y) + p1x)) {
            inside = !inside
        }

        p1x = p2x
        p1y = p2y
        }

        inside
    }

    // Convert matched regions to JSON format
    def resultsToJson(results: List[Results]): String = {
        val jsonList = results.map { matchedRegion =>
        Json.obj(
            "region" -> matchedRegion.region,
            "matched_locations" -> matchedRegion.matchedLocations
        )
        }

        Json.prettyPrint(Json.obj("regions" -> jsonList))
    }
}
