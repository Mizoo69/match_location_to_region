import scala.io.Source
import java.io.PrintWriter
import play.api.libs.json._
import play.api.libs.functional.syntax._

//classes for location, region and results
case class Location(name: String, coordinates: List[Double])
case class Region(name: String, coordinates: List[List[List[Double]]])
case class Results(region: String, matchedLocations: List[String])

object MatchLocationToRegion
{
    def main(args: Array[String]): Unit = {
        val locationFile = args(0)
        val regionFile = args(1)
        val outputFile = args(2)

        //Read json data from the locations and regions files
        val locationsJson = Source.fromFile(locationFile).mkString
        val regionsJson = Source.fromFile(regionFile).mkString

        //Parse json data from the locations and regions files
        val locations = parseLocations(locationsJson)
        val regions = parseRegions(regionsJson)

        //Save results and store them in json
        val results = matchLocationsToRegions(locations, regions)
        val resultsJson = resultsToJson(results)

        //Write to the results file
        val writer = new PrintWriter(outputFile)
        writer.println(resultsJson)
        writer.close()

    }
    
    //parse locations json containing data and return a list of 'location' type objects
    def parseLocations(jsonString: String): List[Location] = {
        val json = Json.parse(jsonString)
        val locationList = json.validate[List[JsObject]]

        locationList.fold(
            errors => {
            println("Failed to parse locations")
            List.empty[Location]
            },
            values => {
                values.flatMap { location =>
                    val name = (location \ "name").validate[String]
                    val coordinates = (location \ "coordinates").validate[List[Double]]

                    (name, coordinates) match {
                    case (JsSuccess(n, _), JsSuccess(c, _)) => Some(Location(n, c))
                    case _ => None
                    }
                }
            }
        )
    }

    //parse regions json containing data and return a list of 'region' type objects
    def parseRegions(jsonString: String): List[Region] = {
        val json = Json.parse(jsonString)
        val regionList = json.validate[List[JsObject]]

        regionList.fold(
            errors => {
            println("Failed to parse regions")
            List.empty[Region]
            },
            values => {
                values.flatMap { region =>
                    val name = (region \ "name").validate[String]
                    val coordinates = (region \ "coordinates").validate[List[List[List[Double]]]]

                    (name, coordinates) match {
                    case (JsError(_), _) | (_, JsError(_)) => None
                    case (JsSuccess(n, _), JsSuccess(c, _)) => Some(Region(n, c))
                    }
                }
            }
        )
    }

    //matches locations with corresponding regions and creates a new list for results
    def matchLocationsToRegions(locations: List[Location], regions: List[Region]): List[Results] = {
        regions.map { region =>
            val matchedLocations = locations.filter(location => isLocationInRegion(location, region))
            Results(region.name, matchedLocations.map(_.name))
        }
    }

    //checks if a location is inside a region
    def isLocationInRegion(location: Location, region: Region): Boolean = {
        val point = location.coordinates
        val polygons = region.coordinates
        polygons.exists(isPointInPolygon(point, _))
    }
    
    //check if location(point) is inside region(polygon)
    def isPointInPolygon(point: List[Double], polygon: List[List[Double]]): Boolean = {
        val x = point(0)
        val y = point(1)
        val n = polygon.length
        var inside = false
        //track 2 consecutive points coordinates
        var p1x = polygon(0)(0)
        var p1y = polygon(0)(1)

        for (i <- 0 until n) {
            //get coordinates of the next point, wrap around to the first if necessary
            val p2x = polygon((i + 1) % n)(0)
            val p2y = polygon((i + 1) % n)(1)
            
            //check if y coordinate is within range of the 2 next consecutive points
            //and x coordinate is to the left of the line connecting the points
            if (((p1y <= y && y < p2y) || (p2y <= y && y < p1y)) &&
                x < ((p2x - p1x) / (p2y - p1y) * (y - p1y) + p1x)) {
            inside = !inside
            }

            p1x = p2x
            p1y = p2y
        }

        inside
    }

    //convert matched regions to json format
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
